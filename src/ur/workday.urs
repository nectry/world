signature AUTH = sig
    val token : transaction (option string)
end

type instance
val read_instance : read instance
val show_instance : show instance

(* Non-opaque type for all Workday IDs. Must be non-opaque to play nice with SQL. *)
type wid = string
val show_wid : show wid
val inj_wid : sql_injectable_prim wid

type worker = {
     Id : wid,
     WName : string, (* name *)
     IsManager : bool,
     PrimaryWorkEmail : option string
}

type directReports = {
     Manager : wid, (* manager *)
     Reports : list wid (* list of worker ids *)
}

type response object = {Data : object, Total : int}

type descriptor = {
     Id : wid,
     Descriptor : option string
}

type comment = {
     Date : time,
     Comment : string,
     Person : descriptor (* who made the comment *)
}
type attachment = {
     Id : wid,
     Description : string,
     ContentType : descriptor,
     FileLength : int,
     UploadDate : time,
     FileName : string,
     UploadedBy : descriptor,
     Category : descriptor
}

type businessProcessParameters = {
     For : option descriptor, (* could be another business process if this is a subprocess *)
     TransactionStatus : option descriptor,
     Action : option descriptor, (* required *)
     OverallBusinessProcess : option descriptor,
     Comment : option string,
     Comments : option (list comment),
     WarningValidations : option string,
     Attachments : option (list attachment),
     OverallStatus : option string,
     CriticalValidations : option string,
}
type feedbackResponse = {
     Id : wid,
     Response : string,
     Declined : bool,
     DeclineReason : string,
     Responder : descriptor,
     Preview : string
}

type feedbackQuestion = {
     Id : wid,
     Question : string,
     Responses : list feedbackResponse,
     QuestionType : descriptor,
     RelatesTo : list descriptor,
     MultipleChoiceAnswers : list descriptor,
     Preview : string
}

type relatedFeedback = {
     Id : wid,
     BusinessProcessParameters : businessProcessParameters,
     Preview : string
}

type anytimeFeedback = {
     Id : wid,
     Preview : option string,
     ToWorker : option descriptor, (* who the feedback is about *)
     WorkersToNotify : option (list descriptor), (* always the direct manager *)
     BusinessProcessParameters : option businessProcessParameters,
     HiddenFromWorker : option bool, (* True if the feedback event is confidential between the feedback provider and the manager. Workers don't see confidential feedback. *)
     HiddenFromManager : option bool, (* True if the feedback event is private between the feedback provider and the worker. Private feedback isn't shared with managers. *)
     Comment : option string, (* actual feedback *)
     FromWorker : option descriptor, (* Identity of the responder. Only provided if the request asked to know the identity of the responder. *)
     OverallStatus : option string,
     GivenDate : option time,
     ShowFeedbackProviderName: option bool,
     (* unused parameters: *)
     (*
     Badge : descriptor,
     Href : string
     AlsoAbout : list descriptor, (* other workers the feedback is about *)
     RelatedFeedbackEvents : list relatedFeedback,
     RelatesTo : descriptor,
      *)
}
val json_anytimeFeedback : Json.json anytimeFeedback

(* We would like to use this type to safely express the possible variants of the
`Status` field of `feedback`, but datatypes cannot be `sql_injectable` so there
is not a good way to map this to strings. *)
(*
datatype feedbackStatus = Complete | Requested
val show_feedbackStatus : show feedbackStatus
val read_feedbackStatus : read feedbackStatus
val eq_feedbackStatus : eq feedbackStatus
*)

type feedback = {
      Wid : option wid,
      Created : time,
      RequestedDate : option time,
      About : wid, (* who the feedback is about *)
      Provider : wid, (* who writes the feedback *)
      RequestedBy : option wid,
      Body : string, (* response *)
      Guidance : string,
      Status : string,
      SendMail : bool,
      LastUpdated : time
}

functor Make(M : AUTH) : sig
    structure Workers : sig
        val get : instance -> wid -> transaction worker
        val getAll : instance -> transaction (list worker)
    end
    structure Feedback : sig
        val get : instance -> wid -> transaction (list anytimeFeedback)
        val getAll : instance -> list wid -> transaction (list anytimeFeedback)
        val post : instance -> feedback -> transaction wid
        val patch : instance -> feedback -> transaction unit
    end
    structure DirectReports : sig
        val get : instance -> wid -> transaction directReports
    end
end
