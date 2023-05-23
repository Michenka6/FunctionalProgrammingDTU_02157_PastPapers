open System
open System.IO
open System.Xml.Serialization

type Person =
    { Name : string
      Age : int }

let xmlString = "<Person><Name>John Doe</Name><Age>30</Age></Person>"

let deserializeXmlString<'T> (xmlString : string) =
    let serializer = new XmlSerializer(typeof<'T>)
    use reader = new StringReader(xmlString)
    serializer.Deserialize(reader) :?> 'T

let person : Person = deserializeXmlString xmlString

printfn "Name: %s, Age: %d" person.Name person.Age