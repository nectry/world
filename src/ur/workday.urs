signature AUTH = sig
    val token : transaction (option string)
end

(* Nonopaque type for all Workday IDs. *)
type wid = string

con worker = [
     Id = wid,
     WName = string, (* name *)
     IsManager = bool,
     PrimaryWorkEmail = option string
]
type directReports = {
     Manager : wid, (* manager *)
     Reports : list wid (* list of worker ids *)
}
type response object = {Data : object}

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

datatype feedbackStatus = Complete | Requested
(*
val show_feedbackStatus : show feedbackStatus
val read_feedbackStatus : read feedbackStatus
val eq_feedbackStatus : eq feedbackStatus
*)

con feedback = [
      Id = wid,
      Created = time,
      RequestedDate = option time,
      About = wid, (* who the feedback is about *)
      Provider = wid, (* who writes the feedback *)
      RequestedBy = option wid,
      Body = string, (* response *)
      Guidance = string,
      Status = string, (* TODO: use feedbackStatus. sql_injectable instance not possible? *)
      SendMail = bool,
      LastUpdated = time
]
con feedbackWithoutId = [
      Created = time,
      RequestedDate = option time,
      About = wid, (* who the feedback is about *)
      Provider = wid, (* who writes the feedback *)
      RequestedBy = option wid,
      Body = string, (* response *)
      Guidance = string,
      Status = string, (* TODO: use feedbackStatus. sql_injectable instance not possible? *)
      SendMail = bool,
      LastUpdated = time
]

table workers : worker
  PRIMARY KEY Id
constraint [Pkey] ~ workers_hidden_constraints

table feedback : feedback
  PRIMARY KEY Id,
  CONSTRAINT About FOREIGN KEY About REFERENCES workers(Id),
  CONSTRAINT Provider FOREIGN KEY Provider REFERENCES workers(Id),
  CONSTRAINT RequestedBy FOREIGN KEY RequestedBy REFERENCES workers(Id)

table directReports : {
      Manager : wid,
      Report : wid
} CONSTRAINT Manager FOREIGN KEY Manager REFERENCES workers(Id),
  CONSTRAINT Report FOREIGN KEY Report REFERENCES workers(Id)

functor Make(M : AUTH) : sig
    structure Workers : sig
        val list : transaction (list $worker)
        val manager : wid -> transaction $worker
        val reports : wid -> transaction (list $worker)
    end
    structure Feedback : sig
        val list : transaction (list $feedback)
        val assigned : transaction (list $feedback)
        val submitted : transaction (list $feedback)

        (*
        val submit : $feedback -> transaction wid
        val request : $feedback -> transaction unit
        *)

        val post : $feedbackWithoutId -> transaction wid
        val patch : unit -> transaction unit
     end
    structure WorkdayApi : sig
        structure WorkersApi : sig
            val get : wid -> transaction $worker
            val getAll : transaction (list $worker)
        end
        structure FeedbackApi : sig
            val get : wid -> transaction (list anytimeFeedback)
            val getAll : transaction (list anytimeFeedback)
            val post : $feedbackWithoutId -> transaction wid
        end
        structure DirectReportsApi : sig
            val get : wid -> transaction directReports
        end
    end
end
