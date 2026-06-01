module MessagePack.Tests.DUTest

open Xunit
open MessagePack

[<MessagePackObject>]
type SimpleUnion =
  | A
  | B of int
  | C of int64 * float32

[<Fact>]
let simple () =

  let input = A
  let actual = convert input
  Assert.Equal(input, actual)

  let input = B 100
  let actual = convert input
  Assert.Equal(input, actual)

  let input = C(99999999L, -123.43f)
  let actual = convert input
  Assert.Equal(input, actual)

type StringKeyUnion = | D of Prop : int

[<Fact>]
let ``string key`` () =

  let input = D 1
  let actual = convert input
  Assert.Equal(input, actual)

// Multi-field named DU fields. The F# compiler prefixes generated factory method
// parameters with '_' (e.g., NewMultiNamed(_first, _second)), which previously
// caused the string-key lookup to fail. This exercises the underscore-stripping fix.
type MultiNamedFieldUnion =
  | Single of value: int
  | Pair   of first: string * second: int
  | Triple of a: int * b: string * c: float

[<Fact>]
let ``multi-field named DU fields round-trip`` () =

  let s = Single(value = 42)
  Assert.Equal(s, convertEq s)

  let p = Pair(first = "hello", second = 99)
  Assert.Equal(p, convertEq p)

  let t = Triple(a = 1, b = "world", c = 3.14)
  Assert.Equal(t, convertEq t)

// Option-typed field inside a DU case.
type UnionWithOption =
  | WithOpt of key: string * detail: string option

[<Fact>]
let ``DU case with option field round-trips`` () =

  let none = WithOpt(key = "k", detail = None)
  Assert.Equal(none, convertEq none)

  let some = WithOpt(key = "k", detail = Some "extra")
  Assert.Equal(some, convertEq some)

let mutable beforeCallback = false
let mutable afterCallback = false

[<MessagePackObject>]
type CallbackUnion =
  | Call
with
  interface IMessagePackSerializationCallbackReceiver with
    override this.OnBeforeSerialize() =
      beforeCallback <- true
    override this.OnAfterDeserialize() =
      afterCallback <- true

[<Fact>]
let ``receive callback`` () =
  let input = Call
  convertEq input |> ignore
  Assert.True(beforeCallback)
  Assert.True(afterCallback)

module Compatibility =

  [<Union(0, typeof<CsA>)>]
  [<Union(1, typeof<CsB>)>]
  [<Union(2, typeof<CsC>)>]
  type CsSimpleUnion = interface end

  and [<MessagePackObject>]CsA() =
    interface CsSimpleUnion

  and [<MessagePackObject>]CsB() =

    [<Key(0)>]
    member val Item: int = 0 with get, set

    interface CsSimpleUnion

  and [<MessagePackObject>]CsC() =

    [<Key(0)>]
    member val Item1: int64 = 0L with get, set

    [<Key(1)>]
    member val Item2: float32 = 0.0f with get, set

    interface CsSimpleUnion

  [<Fact>]
  let simple () =

    let input = A
    let actual = convert<SimpleUnion, CsSimpleUnion> input |> box
    Assert.True(actual :? CsA)

    let input = B 100
    match convert<SimpleUnion, CsSimpleUnion> input |> box with
    | :? CsB as actual ->
      Assert.Equal(100, actual.Item)
    | actual -> Assert.True(false, sprintf "expected: CsB, but was: %A" actual)

    let input = C(99999999L, -123.43f)
    match convert<SimpleUnion, CsSimpleUnion> input  |> box with
    | :? CsC as actual ->
      Assert.Equal(99999999L, actual.Item1)
      Assert.Equal(-123.43f, actual.Item2)
    | actual -> Assert.True(false, sprintf "expected: CsC, but was: %A" actual)

  [<Union(0, typeof<CsD>)>]
  type CsStringKeyUnion = interface end

  and [<MessagePackObject>]CsD() =

    [<Key("Prop")>]
    member val Prop: int = 0 with get, set

    interface CsStringKeyUnion

  [<Fact>]
  let ``string key`` () =

    let input = D 100
    match convert<StringKeyUnion, CsStringKeyUnion> input  |> box with
    | :? CsD as actual ->
      Assert.Equal(100, actual.Prop)
    | actual -> Assert.True(false, sprintf "expected: CsD, but was: %A" actual)
