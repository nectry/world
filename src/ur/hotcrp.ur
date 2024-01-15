open Json

datatype status =
         Inprogress
       | Withdrawn
       | Submitted
       | Accepted
       | Rejected
val _ : json status = json_derived
                          (fn x =>
                              case x of
                                  "inprogress" => Success Inprogress
                                | "withdrawn" => Success Withdrawn
                                | "submitted" => Success Submitted
                                | "Accepted" => Success Accepted
                                | "Rejected" => Success Rejected
                                | "accepted" => Success Accepted
                                | "rejected" => Success Rejected
                                | _ => Failure <xml>Bad HotCRP paper status {[x]}</xml>)
                          (fn x =>
                              case x of
                                  Inprogress => "inprogress"
                                | Withdrawn => "withdrawn"
                                | Submitted => "submitted"
                                | Accepted => "Accepted"
                                | Rejected => "Rejected")

val _ : json time = json_derived'
                        (addSeconds minTime)
                        toSeconds

type author = {
     Email : string,
     First : option string,
     Last : option string,
     Affiliation : option string,
     Contact : option bool
}
val json_author : Json.json author =
    json_record_withOptional {Email = "email"}
    {First = "first",
     Last = "last",
     Affiliation = "affiliation",
     Contact = "contact"}

type submission = {
     Mimetype : string,
     Hash : string,
     Timestamp : time,
     Size : int
}
val json_submission : Json.json submission =
    json_record {Mimetype = "mimetype",
                 Hash = "hash",
                 Timestamp = "timestamp",
                 Size = "size"}

type paper = {
     Pid : int,
     Title : string,
     Status : status,
     Submitted : option bool,
     SubmittedAt : option time,
     Withdrawn : option bool,
     WithdrawnAt : option time,
     Authors : option (list author),
     Abstract : string,
     Submission : option submission,
     Collaborators : option string
}
val json_paper : Json.json paper =
    json_record_withOptional {Pid = "pid",
                              Title = "title",
                              Status = "status",
                              Abstract = "abstract"}
    {Authors = "authors",
     Submitted = "submitted",
     SubmittedAt = "submittedAt",
     Withdrawn = "withdrawn",
     WithdrawnAt = "withdrawnAt",
     Submission = "submission",
     Collaborators = "collaborators"}
