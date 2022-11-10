#light

[<Measure>] type PLN
[<Measure>] type EUR

let convertEurToPln(x: double<EUR>) = x * 5.0



// Define the Address type which is used in the Person record
and Address =
  { Line1: string
    Line2: string
    PostCode: string
    Occupant: Person }
  

type Client = {
    Id: int     
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


