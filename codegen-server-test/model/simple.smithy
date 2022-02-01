$version: "1.0"

namespace com.amazonaws.simple

use aws.protocols#restJson1

@restJson1
@title("SimpleService")
service SimpleService {
    version: "2022-01-01",
    operations: [
        Healthcheck,
    ],
}

@http(uri: "/healthcheck?fooKey=bar", method: "GET")
operation Healthcheck {
    input: HealthcheckInputRequest,
    output: HealthcheckOutputResponse
}

structure HealthcheckInputRequest {
    member: FloatSet
}

structure HealthcheckOutputResponse {
}

set FloatSet {
    member: Float
}
