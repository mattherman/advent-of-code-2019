open System
open System.IO

let getPixels () =
    File.ReadAllText ("input.txt")
    |> Seq.map (Char.GetNumericValue >> int)

let getLayers width height pixels =
    pixels
    |> Seq.chunkBySize height
    |> Seq.chunkBySize width

let countPixels value layer =
    layer
    |> Array.reduce Array.append
    |> Array.filter (fun p -> p = value)
    |> fun values -> (layer, values.Length)

let answer =
    let layers = getPixels () |> getLayers 25 6
    let pixelCountByLayer = layers |> Seq.map (countPixels 0)
    let (layer, count) = pixelCountByLayer |> Seq.minBy snd
    (countPixels 1 layer |> snd) * (countPixels 2 layer |> snd)