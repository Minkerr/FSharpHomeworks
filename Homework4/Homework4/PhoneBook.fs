module Homework4.PhoneBook

open System.IO

let addRecord newRecord book = newRecord :: book

let findPhoneByName name book =
    let resultSeq = Seq.filter (fun x -> fst x = name) book
    if (Seq.length resultSeq = 0) then None
    else Some(snd (Seq.head book))
      
let findNameByPhone phone book =
    let resultSeq = Seq.filter (fun x -> snd x = phone) book
    if (Seq.length resultSeq = 0) then None
    else Some(fst (Seq.head book))

let printBook book = List.iter (fun (name, phone) -> printfn "%s — %s" name phone) book

let writePhoneBookToFile book =
    let mappedBook = book |> List.map (fun record -> (fst record + " — " + snd record))
    File.WriteAllLines ("output.txt", mappedBook)
    
let readPhoneBookFromFile =
    let divideNameAndPhone (line : string) =
        let nameAndPhone = line.Split(" — ")
        if (not (nameAndPhone.Length = 2)) then None
        else Some((nameAndPhone[0], nameAndPhone[1]))
    File.ReadAllLines("input.txt") |> Seq.map divideNameAndPhone
        |> Seq.filter (_.IsSome)
        |> Seq.map (fun x -> match x with | Some x -> x | None -> ("", ""))
        |> Seq.toList
    
