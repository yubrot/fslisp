namespace Fslisp.Core

open System
open System.Linq
open System.Text

[<Struct; CustomEquality; CustomComparison>]
type ByteString =
    private
    | ByteString of ArraySegment<byte>

    static member Create(bytes: byte[]) = ByteString(ArraySegment(bytes))

    static member Create(bytes: byte[], offset: int, count: int) : ByteString =
        ByteString(ArraySegment(bytes, offset, count))

    member self.ArraySegment =
        let (ByteString seg) = self
        seg

    member self.Length = self.ArraySegment.Count

    member self.Item(index) =
        let seg = self.ArraySegment

        if index < 0 || seg.Count <= index then
            raise (IndexOutOfRangeException())
        else
            seg.Array.[seg.Offset + index]

    member self.GetSlice(start, finish) =
        let seg = self.ArraySegment
        let start = defaultArg start 0
        let finish = defaultArg finish seg.Count

        if start < 0 || seg.Count <= finish then
            raise (IndexOutOfRangeException())
        else
            ByteString(ArraySegment(seg.Array, seg.Offset + start, finish + 1 - start))

    member self.Equals(other: ByteString) =
        self.ArraySegment.SequenceEqual(other.ArraySegment)

    member self.CompareTo(other: ByteString) =
        let ByteString x, ByteString y = self, other

        let rec go i =
            let xr = x.Count - i
            let yr = y.Count - i

            if xr = 0 || yr = 0 then
                xr - yr
            else
                let c = x.Array.[x.Offset + i].CompareTo(y.Array.[y.Offset + i])
                if c = 0 then go (i + 1) else c

        go 0

    override self.Equals(other: obj) =
        match other with
        | :? ByteString as other -> self.Equals(other)
        | _ -> false

    override self.GetHashCode() =
        self.ArraySegment |> Seq.fold (fun a b -> a ^^^ b.GetHashCode()) 0

    override self.ToString() =
        sprintf "%A" (Array.ofSeq self.ArraySegment)

    interface IEquatable<ByteString> with
        member a.Equals(b) = a.Equals(b)

    interface IComparable<ByteString> with
        member a.CompareTo(b) = a.CompareTo(b)

    interface IComparable with
        member a.CompareTo(b) = a.CompareTo(b :?> ByteString)

[<RequireQualifiedAccess>]
module ByteString =
    let encode (s: string) : ByteString =
        ByteString.Create(Encoding.UTF8.GetBytes s)

    let decode (ByteString seg) : string =
        Encoding.UTF8.GetString(seg.Array, seg.Offset, seg.Count)

    let concat (ss: ByteString seq) : ByteString =
        ss |> Seq.collect (fun s -> s.ArraySegment) |> Array.ofSeq |> ByteString.Create
