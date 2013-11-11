namespace Microsoft.FSharp.Compiler.Interactive.Hosted
open System
open System.IO
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Interactive

type FsiResultEventArgs(result: string)=
    inherit EventArgs()
    member x.Result = result

type FsiResultEventHandler = delegate of obj * FsiResultEventArgs -> unit

module internal ValueInterpreter =
    // mainly borrowed from NicePrint ... the idea is give back a more friendly represanation of the module/types/values defined
    // but it turns out that's quite hard to do :)
    let getMembers (denv: DisplayEnv) expr =

        let rec isConcreteNamespace x = 
            match x with 
            | TMDefRec(tycons,binds,mbinds,_) -> 
                nonNil tycons || not (FlatList.isEmpty binds) || (mbinds |> List.exists (fun (ModuleOrNamespaceBinding(x,_)) -> not x.IsNamespace))
            | TMDefLet _  -> true
            | TMDefDo _  -> true
            | TMDefs defs -> defs |> List.exists isConcreteNamespace 
            | TMAbstract(ModuleOrNamespaceExprWithSig(_,def,_)) -> isConcreteNamespace def

        let rec imexprLP denv  (ModuleOrNamespaceExprWithSig(_,def,_)) = imdefL denv def

        and imexprL denv (ModuleOrNamespaceExprWithSig(mty,def,m)) = imexprLP denv (ModuleOrNamespaceExprWithSig(mty,def,m))

        and imdefsL denv  x = x |> List.map (imdefL denv)

        and imdefL denv  x = 
            let filterVal    (v:Val) = not v.IsCompilerGenerated && isNone v.MemberInfo
            let filterExtMem (v:Val) = v.IsExtensionMember
            match x with 
            | TMDefRec(_tycons,binds,mbinds,_) -> 
                  (binds |> valsOfBinds |> List.filter filterExtMem |> List.map (fun x -> Some (x.DisplayName, sprintf "%A" x.LiteralValue))) @
                  (binds |> valsOfBinds |> List.filter filterVal    |> List.map (fun x -> Some (x.DisplayName, sprintf "%A" x.LiteralValue))) @
                  (mbinds |> List.map (imbindL denv) |> List.concat)

            | TMDefLet(bind,_) -> ([bind.Var] |> List.filter filterVal    |> List.map (fun x -> Some (x.DisplayName, sprintf "%A" x.LiteralValue)))
            | TMDefs defs -> imdefsL denv defs |> List.concat
            | TMDefDo _  -> [None]
            | TMAbstract mexpr -> imexprLP denv mexpr
        and imbindL (denv: DisplayEnv)  (ModuleOrNamespaceBinding(mspec, def)) = 
            let _nm =  mspec.DemangledModuleOrNamespaceName
            let innerPath = (fullCompPathOfModuleOrNamespace mspec).AccessPath
            let _outerPath = mspec.CompilationPath.AccessPath

            let denv = denv.AddOpenPath (List.map fst innerPath) 
            if mspec.IsNamespace then  
                let basic = imdefL denv def
                basic
            else
                // This is a module 
                let denv  = denv.AddAccessibility mspec.Accessibility 
                let basic = imdefL denv def
                basic
        imexprL denv expr

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

    let readFromOuput = ref true

    let readLoop =    
        async { while !readFromOuput do
                    // at some point we will get a steam disposed exception
                    let readText = 
                        try
                            output'.Flush()
                            output.Flush()
                            output.Read()
                        with
                        // TODO where to write this exception to?
                        | _ -> ""
                    if not (String.IsNullOrWhiteSpace readText) then
                        resultsEvent.Trigger(self, new FsiResultEventArgs(readText))
                    do! Async.Sleep 100 }
                             
    do readLoop |> Async.Start

    // TODO friendly version of the compilation results
    let _toFriendlyResult (declaredImpls, denv) =
        let (TAssembly(declaredImpls)) = declaredImpls
        declaredImpls 
        |> Seq.collect (fun (TImplFile(_qname,_,mexpr,_,_)) -> ValueInterpreter.getMembers denv mexpr)
        |> Seq.choose id
        |> Map.ofSeq
        

    let dispose() =
        readFromOuput := false
        interact.Interrupt()
        interact.Dispose()
        let disposer =  eventLoop :?> IDisposable
        disposer.Dispose()
        input.Dispose()
        output.Dispose()
        input'.Dispose()
        output'.Dispose()

    [<CLIEvent>]
    member self.CompilationResultsText = resultsEvent.Publish
    member self.GotItData = interact.GotIt

    member self.ExecuteScript(script: string) = 
        input.Add (script + ";;" + Environment.NewLine)
        input.Flush()

    member self.Interrupt() = interact.Interrupt()

    interface IDisposable with 
         member self.Dispose() = dispose()

    member self.Dispose() = dispose()
