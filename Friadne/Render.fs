namespace Friadne

open System
open Friadne
open Friadne.Ansi
open Friadne.Symbols

/// Wrap formatted lines in box borders and output string
module Renderer =
  /// Rendering configuration to allow pure/testing-friendly injection
  type RenderConfig =
    { BoxWidth : int
      SourceProvider : string -> Result<string[], string> }

  let defaultConfig : RenderConfig =
    { BoxWidth = 80
      SourceProvider =
        fun fileName ->
          try
            SourceCode.readSourceFile fileName |> Ok
          with ex ->
            Result.Error ex.Message }

  /// Wrap a line: left border + fill to fixed width
  let private wrapWithBorder (content : string) (maxWidth : int) =
    let displayWidth = Ansi.getDisplayWidth content + 1
    let padding = max 0 (maxWidth - displayWidth)
    let paddedContent = content + String (' ', padding)
    Symbols.BoxVerticalWithSpace + paddedContent

  /// Render complete diagnostic information (fixed box width: 80)
  let renderDiagnosticWith (config : RenderConfig) (diagnostic : Diagnostic) : string =
    let lines =
      match config.SourceProvider diagnostic.Location.FileName with
      | Ok content -> content
      | Result.Error msg -> [| $"[Source unavailable: {msg}]" |]

    let boxWidth = config.BoxWidth
    let contentLines = ResizeArray<string> ()

    // Header: code + level + title
    let header =
      sprintf
        "%s %s: %s"
        (Formatter.formatCode diagnostic.Code)
        (Formatter.formatLevel diagnostic.Level)
        diagnostic.Title

    contentLines.Add header

    // Location
    let location = "└─ " + Formatter.formatLocation diagnostic.Location
    contentLines.Add location
    contentLines.Add ""

    // Source code area
    let range = diagnostic.Location.Range
    let startLine = max 1 (range.Start.Line - 1)
    let endLine = min lines.Length (range.End.Line + 1)
    let lineWidth = SourceCode.getLineNumberWidth endLine

    let annotationsByLine =
      diagnostic.Annotations
      |> List.groupBy (fun ann -> ann.Range.Start.Line)
      |> Map.ofList

    seq { startLine..endLine }
    |> Seq.collect (fun lineNum ->
      let lineAnnotations =
        annotationsByLine |> Map.tryFind lineNum |> Option.defaultValue []

      Formatter.formatSourceLine
        lines
        lineNum
        lineWidth
        lineAnnotations
        diagnostic.Level)
    |> Seq.iter contentLines.Add

    contentLines.Add ""

    // Note
    match diagnostic.Note with
    | Some note ->
      let hdr = Ansi.colorAndStyle Ansi.Color.BrightBlue Ansi.Style.Bold "Note"
      contentLines.Add $"○ {hdr}: {note}"
    | None -> ()

    // Help
    match diagnostic.Help with
    | Some help ->
      let hdr = Ansi.colorAndStyle Ansi.Color.BrightGreen Ansi.Style.Bold "Help"
      contentLines.Add $"○ {hdr}: {help}"
    | None -> ()

    // Remove trailing empty lines
    while contentLines.Count > 0
          && String.IsNullOrWhiteSpace (contentLines.[contentLines.Count - 1]) do
      contentLines.RemoveAt (contentLines.Count - 1)

    // Box borders
    let topBorder =
      Symbols.BoxTopLeft.ToString ()
      + String (Symbols.BoxHorizontal, boxWidth)
      + Symbols.BoxTopRight.ToString ()

    let bottomBorder =
      Symbols.BoxBottomLeft.ToString ()
      + String (Symbols.BoxHorizontal, boxWidth)
      + Symbols.BoxBottomRight.ToString ()

    let wrapped =
      contentLines |> Seq.map (fun c -> wrapWithBorder c boxWidth) |> Seq.toList

    let finalLines = [ topBorder ] @ wrapped @ [ bottomBorder ; "" ]
    String.Join (Environment.NewLine, finalLines)

  /// Backward-compatible entry point using default configuration
  let renderDiagnostic (diagnostic : Diagnostic) : string =
    renderDiagnosticWith defaultConfig diagnostic
