signature AUTH = sig
    val token : transaction (option string)
end

(* Opaque type for all Workday IDs. *)
type wid
type worker = {
     Id : wid,
     WName : string, (* name *)
     IsManager : bool,
     PrimaryWorkEmail : string
}
type directReports = {
     Manager : wid, (* manager *)
     Reports : list wid (* list of worker ids *)
}
type response object = {Data : object}

type descriptor = {
     Id : wid,
     Descriptor : string
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
     Id : wid,
     For : descriptor, (* could be another business process if this is a subprocess *)
     TransactionStatus : descriptor,
     Action : descriptor,
     OverallBusinessProcess : descriptor,
     Comment : string,
     Comments : list comment,
     WarningValidations : string,
     Attachments : list attachment,
     OverallStatus : string,
     CriticalValidations : string,
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
     Preview : string,
     ToWorker : descriptor, (* who the feedback is about *)
     WorkersToNotify : list descriptor, (* always the direct manager *)
     BusinessProcessParameters : businessProcessParameters,
     HiddenFromWorker : bool, (* True if the feedback event is confidential between the feedback provider and the manager. Workers don't see confidential feedback. *)
     HiddenFromManager : bool, (* True if the feedback event is private between the feedback provider and the worker. Private feedback isn't shared with managers. *)
     Comment : string, (* actual feedback *)
     FromWorker : descriptor, (* Identity of the responder. Only provided if the request asked to know the identity of the responder. *)
     OverallStatus : string,
     GivenDate : time,
     ShowFeedbackProviderName: bool
     (* unused parameters: *)
     (*
     Badge : descriptor,
     Href : string
     AlsoAbout : list descriptor, (* other workers the feedback is about *)
     RelatedFeedbackEvents : list relatedFeedback,
     RelatesTo : descriptor,
      *)
}

datatype feedbackStatus = Complete | Requested
(* TODO: typeclasses like eq and sql_injectable *)

type feedback = {
      Id : wid,
      Url : string,
      Created : time,
      RequestedDate : time,
      About : wid, (* who the feedback is about *)
      Provider : wid, (* who writes the feedback *)
      RequestedBy : wid,
      Body : string, (* response *)
      Guidance : string,
      Status : feedbackStatus,
      SendMail : bool,
      LastUpdated : time
}

table workers : {
      Id : wid,
      WName : string, (* name *)
      IsManager : bool,
      PrimaryWorkEmail : string
} PRIMARY KEY Id
table feedback : {
      Id : wid,
      Url : string,
      Created : time,
      RequestedDate : time,
      About : wid, (* who the feedback is about *)
      Provider : wid, (* who writes the feedback *)
      RequestedBy : wid,
      Body : string, (* response *)
      Guidance : string,
      Status : feedbackStatus,
      SendMail : bool,
      LastUpdated : time
} PRIMARY KEY Id,
  CONSTRAINT About FOREIGN KEY About REFERENCES workers(Id),
  CONSTRAINT Provider FOREIGN KEY Provider REFERENCES workers(Id),
  CONSTRAINT RequestedBy FOREIGN KEY RequestedBy REFERENCES workers(Id)
table directReports : {
      Manager : wid,
      Report : wid
} CONSTRAINT Manager FOREIGN KEY Manager REFERENCES workers(Id),
  CONSTRAINT Report FOREIGN KEY Report REFERENCES workers(Id)

(* The user-facing version of feedback requests.
   This is what workers see when they check their outstanding feedback
   solicitations.
 *)
type feedbackRequestDisplay = {
     Id : wid, (* hidden in UI *)
     RequestedDate : time,
     About : string,
     Provider : wid,
     RequestedBy : wid,
     Body : string, (* response body *)
     Status : feedbackStatus
}

functor Make(M : AUTH) : sig
    structure Workers : sig
        val list : transaction (list worker)
        val manager : wid -> transaction worker
        val reports : wid -> transaction (list worker)
    end
    structure Feedback : sig
        val list : transaction (list feedbackRequestDisplay)
        val assigned : transaction (list feedback)
        val submitted : transaction (list feedback)
        val completed : transaction (list feedback)
        val submit : feedback -> transaction unit
        val request : feedback -> transaction unit
     end
    structure WorkdayApi : sig
        structure WorkersApi : sig
            val get : wid -> transaction worker
            val getAll : transaction (list worker)
        end
        structure FeedbackApi : sig
            val get : wid -> transaction anytimeFeedback
            val getAll : transaction (list anytimeFeedback)
        end
        structure DirectReportsApi : sig
            val get : wid -> transaction directReports
        end
    end
end
