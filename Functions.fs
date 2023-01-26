module Functions

let generator (a: double) (b: double) (n: int) =
    let step = (b - a) / double n
    let point (num: int) = a + (double num) * step
    point

let rec useFuncGenerator (a: double) (b: double) (n: int) (fu: double -> double) =
    let generateNum = generator a b n
    let rec generateForFunc n =
        if (n <= 0) then
            ()
        else
            let num: double = generateNum (n - 1)
            let f = fu num
            printfn "x: %f y: %f" num f
            generateForFunc (n-1)
    generateForFunc n

let linear (points: list<float * float>) : double -> double =
    let sx = (0.0, points) ||> List.fold (fun acc (x, _) -> acc + x)
    let sxx = (0.0, points) ||> List.fold (fun acc (x, _) -> acc + x * x)
    let sy = (0.0, points) ||> List.fold (fun acc (_, y) -> acc + y)
    let sxy = (0.0, points) ||> List.fold (fun acc (x, y) -> acc + x * y)
    let len = float (points.Length)
    let a = (sxy * len - sx * sy) / (sxx * len - sx * sx)
    let b = (sy - a * sx) / len

    let f x = a * x + b
    f

let logarithm (points: list<float * float>) : double -> double =
    let sx = (0.0, points) ||> List.fold (fun acc (x, _) -> acc + log (x))
    let sy = (0.0, points) ||> List.fold (fun acc (_, y) -> acc + y)
    let sxx = (0.0, points) ||> List.fold (fun acc (x, _) -> acc + (log (x)) ** 2.0)
    let sxy = (0.0, points) ||> List.fold (fun acc (x, y) -> acc + (log (x)) * y)
    let n = float (points.Length)
    let delta = sxx * n - sx * sx
    let delta1 = sxy * n - sx * sy
    let delta2 = sxx * sy - sx * sxy
    let a = delta1 / delta
    let b = delta2 / delta
    let f x = a * log (x) + b
    f

let approx (points: list<float * float>) : double -> double =
    let rec finder count value (points: list<float * float>) =
        if (count <= 0) then
            double 0
        else
            let x, _ = points[count]
            let x2, _ = points[count - 1]

            if (value > x2 && value <= x) then
                let _, y = points[count]
                let _, y2 = points[count - 1]
                let a: double = (y - y2) / (x - x2)
                let b: double = double (y2 - a * x2)
                (a * x + b)
            else
                finder (count - 1) value points

    let f x =
        finder (points.Length - 1) x (points |> List.sort)

    f

let chooseF value (inp: list<float * float>) : double -> double =
    let fu =
        match value with
        | "1" -> linear inp
        | "2" -> logarithm inp
        | "3" -> approx inp
        | _ -> linear inp

    fu
    

let updateList (oldList: (float * float) list) =
    if (oldList.Length > 9) then
        oldList[0..9]
    else oldList



        