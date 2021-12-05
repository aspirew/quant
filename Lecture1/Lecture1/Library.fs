namespace Lecture1

module Lecture1 =
    // zad 2
    let fst (x, _, _) = x
    let mid (_, x, _) = x
    let lst (_, _, x) = x

    // zad 3a
    let rec factorial (n:int64) = 
        match n with
            | n when n < 2L -> 1L
            | n -> n * factorial (n - 1L)

    // zad 3b
    let binomialCoefficient n k = (factorial n) / (factorial k * factorial(n - k))

    // zad 3c
    let pascalTriangle (n:int64) = 
        let rec pascalHelper i l =
            match i with
                | i when i < 1L -> 1L::l
                | i -> pascalHelper (i-1L) ((binomialCoefficient n i)::l)
        pascalHelper n []
    
    
    // zad 4
    type Faculty =
        | PPT
        | IZ
        | IT
        | EFM
        | MAT

    type Student = { StudentName: string; GPA: float; Faculty: Faculty}

    let examples = [
        { StudentName="Marcin"; GPA=2.0; Faculty = PPT };
        { StudentName="Staszek"; GPA=3.12; Faculty = IT };
        { StudentName="Monika"; GPA=4.68; Faculty = IT };
        { StudentName="Olek"; GPA=4.2; Faculty = PPT };
        { StudentName="Gosia"; GPA=3.0; Faculty = IZ };
        { StudentName="Adam"; GPA=5.5; Faculty = IZ };
        { StudentName="Krzysiu"; GPA=5.01; Faculty  = EFM };
        { StudentName="Ania"; GPA=5.07; Faculty = IZ };
        { StudentName="Robert"; GPA=4.55; Faculty = MAT };
        { StudentName="Marcin"; GPA=3.99; Faculty = EFM }
    ]

    let getInfo (s:list<Student>) (f:Faculty) = 
        List.filter (fun x -> x.Faculty.Equals f) s

    let getFacultyAvg (s:list<Student>) (f:Faculty) =
        let studs = getInfo s f
        let rec avgCalc s l =
            match l with
                | h::t -> avgCalc (s+h.GPA) t
                | [] -> s / (float studs.Length)
        avgCalc 0. studs


    //zad 5
    let rec getListWithoutWord s l = 
        match l with
            | h::t -> 
                if h.Equals(s) then t
                else h::(getListWithoutWord s t)
            | [] -> l

    let rec idxList i ls = 
        match ls with
            | h::t -> (i, h)::(idxList (i+1) t)
            | [] -> []