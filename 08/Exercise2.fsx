open System
open System.IO

type PixelColor = Black | White | Transparent
type ImageLayer = int[][]
type Image = PixelColor seq

let toPixelColor pixel =
    match pixel with
    | 0 -> Black
    | 1 -> White
    | _ -> Transparent

let getPixels () =
    File.ReadAllText ("input.txt")
    |> Seq.map (Char.GetNumericValue >> int)

let getLayers width height pixels =
    pixels
    |> Seq.chunkBySize height
    |> Seq.chunkBySize width

let getPixelColor x y (layers: ImageLayer seq) =
    layers
    |> Seq.map (fun layer -> layer.[x].[y] |> toPixelColor)
    |> Seq.find (fun pixel ->
        match pixel with
        | Transparent -> false
        | _ -> true)

let printImage width (image: Image) =
    image
    |> Seq.map (fun pixel ->
        match pixel with
        | Black -> "X"
        | _ -> " ")
    |> Seq.chunkBySize width
    |> Seq.iter (fun line -> 
        Array.iter (fun (pixel: string) -> Console.Write(pixel)) line
        Console.WriteLine())

let image =
    let width = 25
    let height = 6
    let layers = getPixels () |> getLayers width height
    seq {
        for x in 0 .. (width - 1) do
        for y in 0 .. (height - 1) do
        yield getPixelColor x y layers
    }