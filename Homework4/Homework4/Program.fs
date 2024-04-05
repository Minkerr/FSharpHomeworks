open System
open Homework4
open PhoneBook
open System.IO

let printMenu = printf """
    0 — Exit
    1 — Add record
    2 — Find phone by name
    3 — Find name by phone
    4 — Print all records
    5 — Write book to file
    6 — Read book from file
"""

let runPhoneBook =
    let rec readCommand book =
        printMenu
        let command = Console.ReadLine()
        match command with
        | "0" -> exit
        | "1" ->
            printf "Enter the name:\n"
            let name = Console.ReadLine()
            printf "Enter the phone:\n"
            let phone = Console.ReadLine()
            readCommand (addRecord (name, phone) book)
        | "2" ->
            printf "Enter the name:\n"
            let name = Console.ReadLine()
            let result = findPhoneByName name book
            match result with
            |Some(x) -> printf $"The found phone is %s{x}\n"
            |None -> printf "There is no record with such name\n"
            readCommand book
        | "3" ->
            printf "Enter the name:\n"
            let phone = Console.ReadLine()
            let result = findNameByPhone phone book
            match result with
            |Some(x) -> printf $"The found name is %s{x}\n"
            |None -> printf "There is no record with such phone\n"
            readCommand book
        | "4" ->
            printBook book
            readCommand book
        | "5" ->
            printf "Enter the path:\n"
            let path = Console.ReadLine()
            writePhoneBookToFile path book
            readCommand book
        | "6" ->
            printf "Enter the path:\n"
            let path = Console.ReadLine()
            let book = readPhoneBookFromFile path
            readCommand book
        | _ ->
            printf "Incorrect input. Try again\n"
            readCommand book
    readCommand []
        