module Reader

open System
open Functions

let printUseFunc arg =
    match arg with
    | "2" -> printfn "--------------Use-logarithm--------------"
    | "3" -> printfn "--------------Use-Apporx--------------"
    | _ -> printfn "--------------Use-linear--------------"
    

let fn1 (newSeq: (float * float) list) chsFunc a b size = async {
    printUseFunc chsFunc
    let use1 = chooseF chsFunc newSeq
    useFuncGenerator a b size use1
    printfn "--------------------------------------"
}

let fn2 (newSeq: (float * float) list) chsFunc a b size = async {
    printUseFunc chsFunc
    let use1 = chooseF chsFunc newSeq
    useFuncGenerator a b size use1
    printfn "--------------------------------------"
}
let usesFunctions newSeq isTwo chsFunc1 chsFunc2 a b size =
    match isTwo with
    | false ->
        let fc1 = (fn1 newSeq chsFunc1 a b size)
        fc1 |> Async.RunSynchronously
    | true ->
        let fc1 = (fn1 newSeq chsFunc1 a b size) 
        let fc2 = (fn2 newSeq chsFunc2 a b size)
        fc1 |> Async.RunSynchronously
        fc2 |> Async.RunSynchronously
        
        

let handleInputForOne (isTwo: bool) chsFunc1 chsFunc2 size =
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
                    usesFunctions updatedList isTwo chsFunc1 chsFunc2 newA newB size
                    handler updatedList
                    
            else
                handler pList
        else
            ()

    handler []