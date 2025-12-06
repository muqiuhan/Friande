module RendererTests

open Friadne
open Friadne.Renderer
open NUnit.Framework

[<Test>]
let ``renderDiagnosticWith uses injected source provider`` () =
  let lines =
    [| "let x = 1"
       "let y = x + 2" |]

  let config =
    { RenderConfig.BoxWidth = 60
      SourceProvider = fun _ -> Ok lines }

  let diagnostic =
    Diagnostics.createWarning
      "FS"
      1
      "Unused value"
      { FileName = "memory.fsx"
        Range =
          { Start = { Line = 2; Column = 5 }
            End = { Line = 2; Column = 10 } } }
    |> Diagnostics.withAnnotation
      { Start = { Line = 2; Column = 5 }
        End = { Line = 2; Column = 10 } }
      "never used"
      DiagnosticLevel.Warning

  let output = renderDiagnosticWith config diagnostic

  Assert.That(output, Does.Contain("let y = x + 2"))
  Assert.That(output, Does.Contain("[FS01]"))
  Assert.That(output, Does.Contain("Warning"))

[<Test>]
let ``renderDiagnosticWith degrades when source unavailable`` () =
  let config =
    { RenderConfig.BoxWidth = 50
      SourceProvider = fun _ -> Result.Error "missing file" }

  let diagnostic =
    Diagnostics.createError
      "FS"
      2
      "Type error"
      { FileName = "missing.fsx"
        Range =
          { Start = { Line = 1; Column = 1 }
            End = { Line = 1; Column = 1 } } }

  let output = renderDiagnosticWith config diagnostic

  Assert.That(output, Does.Contain("[Source unavailable: missing file]"))

