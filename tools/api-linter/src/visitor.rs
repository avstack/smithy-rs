/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

use crate::context::{ContextStack, ContextType};
use anyhow::{anyhow, Context as _, Result};
use rustdoc_types::{
    Crate, FnDecl, GenericArgs, GenericBound, GenericParamDef, GenericParamDefKind, Generics, Id,
    Item, ItemEnum, ItemSummary, Span, Term, Type, Variant, Visibility, WherePredicate,
};
use smithy_rs_tool_common::macros::here;
use std::collections::HashMap;

const BUILT_IN_CRATES: &[&str] = &["alloc", "core", "std"];

fn warn_unimplemented(name: &str) {
    println!("WARN: `{}` not examined yet.", name);
}

pub struct Visitor {
    root_crate_id: u32,
    root_crate_name: String,
    index: HashMap<Id, Item>,
    paths: HashMap<Id, ItemSummary>,
}

impl Visitor {
    pub fn new(package: Crate) -> Result<Self> {
        Ok(Visitor {
            root_crate_id: Self::root_crate_id(&package)?,
            root_crate_name: Self::root_crate_name(&package)?,
            index: package.index,
            paths: package.paths,
        })
    }

    pub fn visit_all(&self) -> Result<()> {
        let root_context = ContextStack::new(&self.root_crate_name);
        for item in self.index.values() {
            if item.crate_id == self.root_crate_id {
                self.visit_item(&root_context, item)?;
            }
        }
        Ok(())
    }

    fn visit_item(&self, context: &ContextStack, item: &Item) -> Result<()> {
        if !matches!(item.visibility, Visibility::Public) {
            return Ok(());
        }

        let root_level = context.is_root();
        let mut context = context.clone();
        match &item.inner {
            ItemEnum::AssocConst { .. } => warn_unimplemented("assoc_const"),
            ItemEnum::AssocType { .. } => warn_unimplemented("assoc_type"),
            ItemEnum::Constant(_) => {
                context.push(ContextType::Constant, item);
                warn_unimplemented("constant")
            }
            ItemEnum::Enum(enm) => {
                context.push(ContextType::Enum, item);
                self.visit_generics(&context, &enm.generics)?;
                for id in &enm.impls {
                    self.visit_impl(&context, self.item(id)?)?;
                }
            }
            ItemEnum::ForeignType => warn_unimplemented("foreign_type"),
            ItemEnum::Function(function) => {
                context.push(ContextType::Function, item);
                self.visit_fn_decl(&context, &function.decl)?;
            }
            ItemEnum::Import(import) => {
                if let Some(id) = &import.id {
                    context.push_raw(ContextType::Reexport, &import.name, item.span.as_ref());
                    self.check_thirdparty(&context, "re-export named", id)
                        .context(here!())?;
                }
            }
            ItemEnum::Method(method) => {
                // methods don't exist at the root level
                if !root_level {
                    context.push(ContextType::Method, item);
                    self.visit_fn_decl(&context, &method.decl)?;
                }
            }
            ItemEnum::Module(module) => {
                if !module.is_crate {
                    context.push(ContextType::Module, item);
                }
                for id in &module.items {
                    let module_item = self.item(id)?;
                    self.visit_item(&context, module_item)?;
                }
            }
            ItemEnum::OpaqueTy(_) => warn_unimplemented("opaque_ty"),
            ItemEnum::Static(_) => warn_unimplemented("static"),
            ItemEnum::Struct(strct) => {
                context.push(ContextType::Struct, item);
                self.visit_generics(&context, &strct.generics)?;
                for id in &strct.fields {
                    let field = self.item(id)?;
                    self.visit_item(&context, field)?;
                }
                for id in &strct.impls {
                    self.visit_impl(&context, self.item(id)?)?;
                }
            }
            ItemEnum::StructField(typ) => {
                // struct fields don't exist at the root level
                if !root_level {
                    self.visit_type(&context, "struct field of", typ)
                        .context(here!())?;
                }
            }
            ItemEnum::Trait(trt) => {
                context.push(ContextType::Trait, item);
                self.visit_generics(&context, &trt.generics)?;
                self.visit_generic_bounds(&context, &trt.bounds)?;
                for id in &trt.items {
                    let field = self.item(id)?;
                    self.visit_item(&context, field)?;
                }
            }
            ItemEnum::Typedef(typedef) => {
                self.visit_type(&context, "typedef type of", &typedef.type_)
                    .context(here!())?;
                self.visit_generics(&context, &typedef.generics)?;
            }
            ItemEnum::TraitAlias(_) => warn_unimplemented("trait_alias"),
            ItemEnum::Union(_) => warn_unimplemented("union"),
            ItemEnum::Variant(variant) => {
                // enum variants don't exist at the root level
                if !root_level {
                    self.visit_variant(&context, item, variant)?;
                }
            }
            ItemEnum::ExternCrate { .. }
            | ItemEnum::Impl(_)
            | ItemEnum::Macro(_)
            | ItemEnum::PrimitiveType(_)
            | ItemEnum::ProcMacro(_) => {}
        }
        Ok(())
    }

    fn visit_impl(&self, context: &ContextStack, item: &Item) -> Result<()> {
        if let ItemEnum::Impl(imp) = &item.inner {
            // Ignore blanket implementations
            if imp.blanket_impl.is_some() {
                return Ok(());
            }
            self.visit_generics(context, &imp.generics)?;
            for id in &imp.items {
                self.visit_item(context, self.item(id)?)?;
            }
            if let Some(trait_) = &imp.trait_ {
                self.visit_type(context, "implemented trait of", trait_)
                    .context(here!())?;
            }
        } else {
            unreachable!("should be passed an Impl item");
        }
        Ok(())
    }

    fn visit_fn_decl(&self, context: &ContextStack, decl: &FnDecl) -> Result<()> {
        for (index, (name, typ)) in decl.inputs.iter().enumerate() {
            if index == 0 && name == "self" {
                continue;
            }
            self.visit_type(context, &format!("argument named `{}` of", name), typ)
                .context(here!())?;
        }
        if let Some(output) = &decl.output {
            self.visit_type(context, "return value of", output)
                .context(here!())?;
        }
        Ok(())
    }

    fn visit_type(&self, context: &ContextStack, what: &str, typ: &Type) -> Result<()> {
        match typ {
            Type::ResolvedPath {
                id,
                args,
                param_names,
                ..
            } => {
                self.check_thirdparty(context, what, id).context(here!())?;
                if let Some(args) = args {
                    self.visit_generic_args(context, args)?;
                }
                self.visit_generic_bounds(context, param_names)?;
            }
            Type::Generic(_) => {}
            Type::Primitive(_) => {}
            Type::FunctionPointer(fp) => {
                self.visit_fn_decl(context, &fp.decl)?;
                self.visit_generic_param_defs(context, &fp.generic_params)?;
            }
            Type::Tuple(types) => {
                for (index, typ) in types.iter().enumerate() {
                    self.visit_type(context, &format!("{}: tuple index {}", what, index), typ)?;
                }
            }
            Type::Slice(typ) => self.visit_type(context, what, typ).context(here!())?,
            Type::Array { type_, .. } => self.visit_type(context, what, type_).context(here!())?,
            Type::ImplTrait(impl_trait) => self.visit_generic_bounds(context, impl_trait)?,
            Type::Infer => warn_unimplemented("Type::Infer"),
            Type::RawPointer { type_: _, .. } => warn_unimplemented("Type::RawPointer"),
            Type::BorrowedRef { type_, .. } => {
                self.visit_type(context, what, type_).context(here!())?
            }
            Type::QualifiedPath {
                self_type, trait_, ..
            } => {
                self.visit_type(context, "qualified self type", self_type)?;
                self.visit_type(context, "qualified type `as` trait", trait_)?;
            }
        }
        Ok(())
    }

    fn visit_generic_args(&self, context: &ContextStack, args: &GenericArgs) -> Result<()> {
        match args {
            GenericArgs::AngleBracketed { bindings, .. } => {
                for binding in bindings {
                    match &binding.binding {
                        rustdoc_types::TypeBindingKind::Equality(term) => {
                            if let Term::Type(typ) = term {
                                self.visit_type(context, "generic default binding of", typ)
                                    .context(here!())?;
                            }
                        }
                        rustdoc_types::TypeBindingKind::Constraint(bounds) => {
                            self.visit_generic_bounds(context, bounds)?;
                        }
                    }
                }
            }
            GenericArgs::Parenthesized { inputs, output } => {
                for input in inputs {
                    self.visit_type(context, "closure input of", input)
                        .context(here!())?;
                }
                if let Some(output) = output {
                    self.visit_type(context, "closure output of", output)
                        .context(here!())?;
                }
            }
        }
        Ok(())
    }

    fn visit_generic_bounds(&self, context: &ContextStack, bounds: &[GenericBound]) -> Result<()> {
        for bound in bounds {
            if let GenericBound::TraitBound {
                trait_,
                generic_params,
                ..
            } = bound
            {
                self.visit_type(context, "trait bound of", trait_)
                    .context(here!())?;
                for param_def in generic_params {
                    match &param_def.kind {
                        GenericParamDefKind::Type { bounds, default } => {
                            self.visit_generic_bounds(context, bounds)?;
                            if let Some(default) = default {
                                self.visit_type(context, "generic default binding of", default)
                                    .context(here!())?;
                            }
                        }
                        GenericParamDefKind::Const { ty, .. } => {
                            self.visit_type(context, "generic default binding of", ty)
                                .context(here!())?;
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_generic_param_defs(
        &self,
        context: &ContextStack,
        params: &[GenericParamDef],
    ) -> Result<()> {
        for param in params {
            match &param.kind {
                GenericParamDefKind::Type { bounds, default } => {
                    self.visit_generic_bounds(context, bounds)?;
                    if let Some(typ) = default {
                        self.visit_type(context, "generic default binding of", typ)
                            .context(here!())?;
                    }
                }
                GenericParamDefKind::Const { ty, .. } => {
                    self.visit_type(context, "const generic type of", ty)
                        .context(here!())?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn visit_generics(&self, context: &ContextStack, generics: &Generics) -> Result<()> {
        self.visit_generic_param_defs(context, &generics.params)?;
        for where_pred in &generics.where_predicates {
            match where_pred {
                WherePredicate::BoundPredicate { ty, bounds } => {
                    self.visit_type(context, "where bound predicate of", ty)
                        .context(here!())?;
                    self.visit_generic_bounds(context, bounds)?;
                }
                WherePredicate::RegionPredicate { bounds, .. } => {
                    self.visit_generic_bounds(context, bounds)?;
                }
                WherePredicate::EqPredicate { lhs, .. } => {
                    self.visit_type(context, "where eq predicate of", lhs)
                        .context(here!())?;
                }
            }
        }
        Ok(())
    }

    fn visit_variant(&self, context: &ContextStack, item: &Item, variant: &Variant) -> Result<()> {
        match variant {
            Variant::Plain => {}
            Variant::Tuple(types) => {
                for typ in types {
                    let mut variant_context = context.clone();
                    variant_context.push(ContextType::EnumVariant, item);
                    self.visit_type(context, "type in enum tuple of", typ)?;
                }
            }
            Variant::Struct(_ids) => warn_unimplemented("enum struct fields"),
        }
        Ok(())
    }

    fn check_thirdparty(&self, context: &ContextStack, what: &str, id: &Id) -> Result<()> {
        if let Some(crate_name) = self.crate_name(id) {
            if self.root_crate_name != crate_name
                && !BUILT_IN_CRATES.iter().any(|&name| name == crate_name)
            {
                let location = if let Some(span) = context.last_span() {
                    format!(" at {}", Self::location(span))
                } else {
                    "".to_string()
                };
                println!(
                    "Unapproved external type `{}` referenced in public API: {} `{}`{}",
                    self.type_name(id)?,
                    what,
                    context,
                    location
                );
            }
        }
        // Crates like `pin_project` do some shenanigans to create and reference types that don't end up
        // in the doc index, but that should only happen within the root crate.
        else if !id.0.starts_with(&format!("{}:", self.root_crate_id)) {
            unreachable!("A type is referencing another type that is not in the index, and that type is from another crate.");
        }
        Ok(())
    }

    fn location(span: &Span) -> String {
        format!(
            "{}:{}:{}",
            span.filename.to_string_lossy(),
            span.begin.0,
            span.begin.1
        )
    }

    fn item(&self, id: &Id) -> Result<&Item> {
        self.index
            .get(id)
            .ok_or_else(|| anyhow!("Failed to find item in index for ID {:?}", id))
            .context(here!())
    }

    fn item_summary(&self, id: &Id) -> Option<&ItemSummary> {
        self.paths.get(id)
    }

    fn crate_name(&self, id: &Id) -> Option<&str> {
        self.item_summary(id)?.path.get(0).map(|s| s.as_str())
    }

    fn type_name(&self, id: &Id) -> Result<String> {
        Ok(self.item_summary(id).context(here!())?.path.join("::"))
    }

    fn root_crate_id(package: &Crate) -> Result<u32> {
        Ok(Self::root(package)?.crate_id)
    }

    fn root_crate_name(package: &Crate) -> Result<String> {
        Ok(Self::root(package)?
            .name
            .as_ref()
            .expect("root should always have a name")
            .clone())
    }

    fn root(package: &Crate) -> Result<&Item> {
        package
            .index
            .get(&package.root)
            .ok_or_else(|| anyhow!("root not found in index"))
            .context(here!())
    }
}
