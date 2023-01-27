module Reader

open System
open Functions


let usesFunctions newSeq (chsFunc1: string) (chsFunc2: string) a b size =
    if (chsFunc2 = "") then
        let fc = chooseF chsFunc1 newSeq
        let res = fc |> Async.RunSynchronously
        useFuncGenerator a b size res
        printfn "--------------------------"
    else
        let fcs = seq {chsFunc1; chsFunc2}
        let mySeq =
            fcs |> Seq.map (fun el -> (chooseF el newSeq))
            |> Async.Parallel
            |> Async.RunSynchronously
            
        mySeq |> Seq.iter(fun el ->
            useFuncGenerator a b size el
            printfn "--------------------------"
            )
        

let handleInputForOne (chsFunc1: string) (chsFunc2: string) size =
    let rec handler pList =
        let line = Console.ReadLine()

        if (not (isNull line) && line <> "") then
            let data = line.Split(";")

            if data.Length >= 2 then
                let x = float data[0]
                let y = float data[1]

                let newPlist =
                    match pList with
                    | [] -> [ (x, y) ]
                    | _ -> (x, y) :: pList
                    
                if newPlist.Length < 10 then
                    handler newPlist
                else
                    let updatedList = updateList newPlist
                    let newA, _ = updatedList[0]
                    let newB, _ = updatedList[1]
                    usesFunctions updatedList chsFunc1 chsFunc2 newA newB size
                    handler updatedList
                    
            else
                handler pList
        else
            ()

    handler []