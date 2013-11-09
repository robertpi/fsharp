namespace Microsoft.FSharp.Compiler.Interactive.Hosted
open System
open System.IO
open Microsoft.FSharp.Compiler.Interactive

type FsiResultEventArgs(result: string)=
    inherit EventArgs()
    member x.Result = result

type FsiResultEventHandler = delegate of obj * FsiResultEventArgs -> unit

type FsiHosted() as self =
    let input = new Shell.CompilerInputStream()
    let output = new Shell.CompilerOutputStream()
    
    let input', output' = new StreamReader(input), new StreamWriter(output)
    let args = [|"fsi.exe"; "--nologo"; "--readline-"; "--gui-"|]

    let resultsEvent = new Event<FsiResultEventHandler, FsiResultEventArgs>()

    let eventLoop = RuntimeHelpers.GetSimpleEventLoop()
    
    let interact = 
            new Shell.FsiEvaluationSession(args, input', output', output', eventLoop)
    let runAsync =
        async { do interact.Run() }

    do Async.Start runAsync

    let readLoop =    
        async { while true do
                    do output'.Flush()
                    do output.Flush()
                    let readText = output.Read()
                    if not (String.IsNullOrWhiteSpace readText) then
                        resultsEvent.Trigger(self, new FsiResultEventArgs(readText))
                    do! Async.Sleep 100 }
                             
    do readLoop |> Async.Start

    [<CLIEvent>]
    member self.Results = resultsEvent.Publish

    member self.ExecuteScript(script: string) = 
        input.Add (script + ";;" + Environment.NewLine)
        input.Flush()

    member self.Interrupt() = interact.Interrupt()

    interface IDisposable with 
         member self.Dispose() =
            interact.Interrupt()
            let disposer =  eventLoop :?> IDisposable
            disposer.Dispose()

