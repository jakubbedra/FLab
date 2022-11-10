open System
open System.IO
open Newtonsoft.Json

[<Measure>]
type PLN

[<Measure>]
type EUR

let convertEurToPln = LanguagePrimitives.DecimalWithMeasure<PLN/EUR>(5.0m)
let convertPlnToEur = LanguagePrimitives.DecimalWithMeasure<EUR/PLN>(0.2m)

let promotionThresholdPln = 2137m<PLN>
let promotionThresholdDays: int = 20

type ClientType =
    | Basic
    | VIP

// Define the Address type which is used in the Person record

type Client =
    { Id: int
      MaxCredit: decimal<PLN>
      RegistrationDate: DateTime
      Person: Person option
      Type: ClientType }

and Person =
    { Name: string
      Surname: string
      PESEL: string option }

type Transaction = { cost: decimal<EUR>; paid: bool }

type ClientTransactions =
    { ClientId: int
      Transactions: list<Transaction> }

let person0 =
    { Name = "Adam"
      Surname = "Małysz"
      PESEL = Some "1234567890" }

let person1 =
    { Name = "Mariusz"
      Surname = "Pudzianowski"
      PESEL = Some "1111111111" }

let person2 =
    { Name = "Marek"
      Surname = "Kubale"
      PESEL = Some "9999999999" }

let client0 =
    { Id = 0
      Type = Basic
      MaxCredit = 10000m<PLN>
      RegistrationDate = DateTime.Now
      Person = Some person0 }

let client1 =
    { Id = 1
      Type = Basic
      MaxCredit = 10m<PLN>
      RegistrationDate = new DateTime(2020, 1, 1)
      Person = Some person1 }

let client2 =
    { Id = 2
      Type = VIP
      MaxCredit = 9999999m<PLN>
      RegistrationDate = new DateTime(1900, 1, 1)
      Person = Some person2 }

let client3 =
    { Id = 3
      Type = Basic
      MaxCredit = 0m<PLN>
      RegistrationDate = new DateTime(2005, 4, 2)
      Person = None }

let clients: list<Client> =
    [ client0; client1; client2; client3 ]

let readClients () =
    File.ReadAllText "clients.json"
    |> JsonConvert.DeserializeObject<list<Client>>

let updateClients clientsList =
    File.WriteAllText("clients.json", JsonConvert.SerializeObject clientsList)

let readTransactions () =
    File.ReadAllText "transactions.json"
    |> JsonConvert.DeserializeObject<list<ClientTransactions>>

let updateTransactions (transactions: list<ClientTransactions>) =
    File.WriteAllText("transactions.json", JsonConvert.SerializeObject transactions)

let getTransactionsList clientId =
    let areOfClient transactions = transactions.ClientId = clientId
    readTransactions () |> Seq.tryFind areOfClient

let findClient id =
    let sameId client = client.Id = id
    readClients () |> Seq.tryFind sameId

let rec updateClient (client: Client) =
    readClients ()
    |> Seq.filter (fun x -> x.Id <> client.Id)
    |> Seq.append [ client ]
    |> updateClients

let isOurClientForLongerThan (clientId: int, days: int) =
    let client = findClient clientId

    match client with
    | Some cl -> (DateTime.UtcNow - cl.RegistrationDate).Days > days
    | None -> false

let increaseCredit (clientId, credit) =
    let client = findClient clientId

    match client with
    | Some cl ->
        Some
            { Id = cl.Id
              Type = cl.Type
              MaxCredit = cl.MaxCredit + credit
              RegistrationDate = cl.RegistrationDate
              Person = cl.Person }
    | None -> None

type public Shop() =
    member public this.promoteClient(clientId: int) =
        if isOurClientForLongerThan (clientId, promotionThresholdDays)
            && this.getTotalMoneySpend(clientId) > promotionThresholdPln
        then
            this.mapToPromotedClient clientId
        else
            None

    member private this.getClientTransactions(clientId: int) =
        readTransactions ()
        |> Seq.tryFind (fun c -> c.ClientId = clientId)
        
    member private this.getTotalMoneySpend(clientId: int) =
        match this.getClientTransactions clientId with
            | Some t ->
                let total = t.Transactions
                            |> Seq.filter (fun tr -> tr.paid)
                            |> Seq.sumBy (fun tr -> tr.cost)
                total * convertEurToPln     
            | None -> 0m<PLN>
        
    member private this.mapToPromotedClient(clientId: int) =
        match findClient clientId with
        | Some cl ->
            Some
                { Id = cl.Id
                  Type = VIP
                  MaxCredit = cl.MaxCredit
                  RegistrationDate = cl.RegistrationDate
                  Person = cl.Person }
        | None -> None

// main
updateClients clients

//printfn $"{clients}"
let shop = new Shop()

//updateClient (shop.promoteClient(0))

let clientsList = readClients ()

printfn "%A" clientsList
