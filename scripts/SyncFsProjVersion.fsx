#r "nuget: FSharp.Data"
open FSharp.Data
open System.Xml.Linq
open System.IO

type PackageJson = JsonProvider<"../package.json">
type Fsproj = XmlProvider<"../fable-effect.fsproj">
let fsproj = Fsproj.Load "../fable-effect.fsproj"
let packageJson = PackageJson.GetSample ()

let version = packageJson.Version
fsproj.PropertyGroup.XElement.Element(XName.Get "Version").Value <- version
fsproj.PropertyGroup.XElement.Element(XName.Get "PackageVersion").Value <- version
fsproj.XElement.Save("fable-effect.fsproj")
fsproj.ToString() |> printfn "%s"
