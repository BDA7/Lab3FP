# Функциональное программирование
## Лабораторная работа 3

**Выполнил:** Бондаренко Данила Александрович \
**Группа:** P34112 \
**Преподаватель:** Пенской Александр Владимирович

### Реализация
Функция читает из стандартного потока пока не будет пустая строка.
В результате получаем список пар (x,y), если в листе уже 10 элементов,
то обновляем его убирая самый старый, потом применяем функцию для
вычисления
```f#
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
```
Функция для выбора количества аппроксимаций, если их две, то вычисления выполняются параллельно
```f#
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
```
Полученный список точек обрабатывается, вычисляется функция. Ожидается ввод новой пары чисел и алгоритм повторяется.\
В зависимости от аргументов командной строки, выбирается аппроксимирующая функция
```f#
let chooseF value (inp: list<float * float>) = async {
    let fu =
        match value with
        | "1" -> linear inp
        | "2" -> logarithm inp
        | "3" -> approx inp
        | _ -> linear inp

    return fu
}

```

Аппроксимация линейной функции.
```f#
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
```
Аппроксимация отрезками.
```f#
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
```
Аппроксимация логарифма
```f#
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
```
Генератор точек. a, b и n передаются аргументами при запуске.
```f#
let generator (a: double) (b: double) (n: int) =
    let step = (b - a) / double n
    let point (num: int) = a + (double num) * step
    point
```
Выводим результат функции на сгенерировнной последовательности
```f#
let fn1 (newSeq: (float * float) list) chsFunc a b size = async {
    printUseFunc chsFunc
    let use1 = chooseF chsFunc newSeq
    useFuncGenerator a b size use1
    printfn "--------------------------------------"
}
```

### Выводы
Данная лабораторная работа заставила обдумать множество вещей в особенности моментов работы ввода/вывода, было больно и сложно, но стараемся держаться.
