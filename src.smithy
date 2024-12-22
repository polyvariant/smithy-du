$version: "2"

namespace test

structure Hello {
    foo: Foo
}

structure Foo {
    bar: Bar
}

list Bar {
    member: String
}

structure Hello2 {
    foo: Foo
    bar: Integer
}

service FooService {
    operations: [
        GetFoo
    ]
}

@readonly
@http(method: "GET", uri: "/foo")
operation GetFoo {
    output := {
        @required
        @httpPayload
        foo: Foo
    }
}

structure Rec {
    rec2: Rec2
}

structure Rec2 {
    rec3: Rec3
}

structure Rec3 {
    rec: Rec
}
