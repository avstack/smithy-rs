/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0.
 */

use crate::visitor::Visitor;
use anyhow::{Context, Result};
use clap::Parser;
use rustdoc_types::FORMAT_VERSION;
use smithy_rs_tool_common::macros::here;
use smithy_rs_tool_common::shell::ShellOperation;
use std::path::PathBuf;

mod cargo;
mod context;
mod visitor;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Args {
    /// Path to the crate to examine
    #[clap(long)]
    crate_path: PathBuf,
    /// Expected `target/` directory for that crate
    #[clap(long)]
    target_path: PathBuf,
}

fn main() -> Result<()> {
    let args = Args::parse();

    println!(
        "This build understands rustdoc format version {}",
        FORMAT_VERSION
    );
    println!("Running rustdoc to produce json doc output...");
    let package = cargo::CargoRustDocJson::new(args.crate_path, args.target_path)
        .run()
        .context(here!())?;

    println!("Examining all public types...");
    Visitor::new(package)?.visit_all()?;

    Ok(())
}
