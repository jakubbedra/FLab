#light

open System

[<Measure>] type PLN
[<Measure>] type EUR

let convertEurToPln(x: decimal<EUR>) = x * 5.0m



// Define the Address type which is used in the Person record

type Client = {
    Id: int
    MaxCredit: decimal // PLN
    RegistrationDate: DateTime
    Person: Person option
}
and Person = {
    Name: string
    Surname: string
    PESEL: int option
}

  
let rec person =
{
    Name = "Person name"
    Age = 12
    Address =
    {
        Line1 = "line 1"
        Line2 = "line 2"
        PostCode = "abc123"
        Occupant = person
    }
}


printfn "Hello from F#"


