#r "nuget: FSharp.Data"
open FSharp.Data
open System.Xml.Linq
open System.IO
open System.Diagnostics

type PackageJson = JsonProvider<"../package.json">
type Fsproj = XmlProvider<"../fable-effect.fsproj">
let fsproj = Fsproj.Load "../fable-effect.fsproj"
let packageJson = PackageJson.GetSample ()

let version = packageJson.Version
fsproj.PropertyGroup.XElement.Element(XName.Get "Version").Value <- version
fsproj.PropertyGroup.XElement.Element(XName.Get "PackageVersion").Value <- version
fsproj.XElement.Save("fable-effect.fsproj")
fsproj.ToString() |> printfn "%s"

let commit message = 
  let startInfo = ProcessStartInfo(
    FileName = "git",
    Arguments = sprintf "commit -a -m \"%s\"" message
  )
  startInfo.RedirectStandardError <- true
  startInfo.RedirectStandardOutput <- true
  startInfo.UseShellExecute <- false
  startInfo.CreateNoWindow <- true
  use p = new Process(StartInfo = startInfo)
  p.Start() |> ignore
  let output = p.StandardOutput.ReadToEnd()
  let error = p.StandardError.ReadToEnd()
  p.WaitForExit()

  match p.ExitCode with
  | 0 -> Ok output
  | code -> Error (code, output, error)


let tag = 
  let startInfo = ProcessStartInfo(
    FileName = "pnpm",
    Arguments = "exec changeset tag"
  )
  startInfo.RedirectStandardError <- true
  startInfo.RedirectStandardOutput <- true
  startInfo.UseShellExecute <- false
  startInfo.CreateNoWindow <- true
  use p = new Process(StartInfo = startInfo)
  p.Start() |> ignore
  let output = p.StandardOutput.ReadToEnd()
  let error = p.StandardError.ReadToEnd()
  p.WaitForExit()

  match p.ExitCode with
  | 0 -> Ok output
  | code -> Error (code, output, error)

commit (sprintf "Release v%s" version)
tag

