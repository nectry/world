open Json
(* open Bootstrap *)

(* FIXME *)
(*
structure Scope = struct
    type t = Scopes.t [UserProfile, MeetingRead, MeetingWrite, WebinarRead, WebinarWrite, DashboardMeetingsRead]
    val empty = Scopes.empty
    val union = Scopes.union
    val toString = Scopes.toString {UserProfile = "user_profile",
                                    MeetingRead = "meeting:read",
                                    MeetingWrite = "meeting:write",
                                    WebinarRead = "webinar:read",
                                    WebinarWrite = "webinar:write",
                                    DashboardMeetingsRead = "dashboard_meetings:read:admin"}

    val userProfile = Scopes.one [#UserProfile]
    val meetingRead = Scopes.one [#MeetingRead]
    val meetingWrite = Scopes.one [#MeetingWrite]
    val webinarRead = Scopes.one [#WebinarRead]
    val webinarWrite = Scopes.one [#WebinarWrite]
    val dashboardMeetingsRead = Scopes.one [#DashboardMeetingsRead]

    val readonly = Scopes.disjoint (union meetingWrite webinarWrite)
end
                  *)

signature AUTH = sig
    val token : transaction (option string)
end

(* Nonopaque type for all Workday IDs. *)
type wid = string

con worker = [
     Id = wid,
     WName = string, (* name *)
     IsManager = bool,
     PrimaryWorkEmail = string
]
val _ : json $worker = json_record
                      {Id = "id",
                       WName = "descriptor", (* name *)
                       IsManager = "isManager",
                       PrimaryWorkEmail = "primaryWorkEmail"}

type directReports = {
     Manager : wid, (* manager *)
     Reports : list wid (* list of worker ids *)
}

(* could be anonymous instance by inlining the record structure *)
type response object = {Data : object}
fun json_response [object] (_ : json object) : json (response object) =
    json_record {Data = "data"}

(* Generic way to refer to other objects by ID and provide a short preview string. *)
type descriptor = {
     Id : wid,
     Descriptor : string
}
val _ : json descriptor =
    json_record {Id = "id",
                 Descriptor = "descriptor"}

type comment = {
     Date : time,
     Comment : string,
     Person : descriptor (* who made the comment *)
}
val _ : json comment =
    json_record {Date = "commentDate",
                 Comment = "comment",
                 Person = "person"}

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
val _ : json attachment =
    json_record {Id = "id",
                 Description = "description",
                 ContentType = "contentType",
                 FileLength = "fileLength",
                 UploadDate = "uploadDate",
                 FileName = "fileName",
                 UploadedBy = "uploadedBy",
                 Category = "category"}

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
val _ : json businessProcessParameters =
    json_record {Id = "id",
                 For = "for",
                 TransactionStatus = "transactionStatus",
                 Action = "action",
                 OverallBusinessProcess = "overallBusinessProcess",
                 Comment = "comment",
                 Comments = "comments",
                 WarningValidations = "warningValidations",
                 Attachments = "attachments",
                 OverallStatus = "overallStatus",
                 CriticalValidations = "criticalValidations"}

(* Individual response to feedback questions. *)
type feedbackResponse = {
     Id : wid,
     Response : string,
     Declined : bool,
     DeclineReason : string,
     Responder : descriptor,
     Preview : string
}
val _ : json feedbackResponse =
    json_record {Id = "id",
                 Response = "response",
                 Declined = "feedbackDeclined",
                 DeclineReason = "feedbackDeclineReason",
                 Responder = "feedbackResponder",
                 Preview = "descriptor"}

type feedbackQuestion = {
     Id : wid,
     Question : string,
     Responses : list feedbackResponse,
     QuestionType : descriptor,
     RelatesTo : list descriptor,
     MultipleChoiceAnswers : list descriptor,
     Preview : string
}
val _ : json feedbackQuestion =
    json_record {Id = "id",
                 Question = "question",
                 Responses = "feedbackResponses",
                 QuestionType = "questionType",
                 RelatesTo = "relatesTo",
                 MultipleChoiceAnswers = "questionMultipleChoiceAnswers",
                 Preview = "descriptor"}

type relatedFeedback = {
     Id : wid,
     BusinessProcessParameters : businessProcessParameters,
     Preview : string
}
val _ : json relatedFeedback =
    json_record {Id = "id",
                 Preview = "descriptor",
                 BusinessProcessParameters = "businessProcessParameters"}

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
val _ : json anytimeFeedback =
    json_record {Id = "id",
                 Preview = "descriptor",
                 ToWorker = "toWorker",
                 WorkersToNotify = "workersToNotify",
                 BusinessProcessParameters = "businessProcessParameters",
                 HiddenFromWorker = "hiddenFromWorker",
                 HiddenFromManager = "hiddenFromManager",
                 Comment = "comment",
                 FromWorker = "fromWorker",
                 OverallStatus = "overallStatus",
                 GivenDate = "feedbackGivenDate",
                 ShowFeedbackProviderName = "showFeedbackProviderName"
                 (*
                 RelatesTo = "relatesTo",
                 Badge = "badge",
                 AlsoAbout = "feedbackAlsoAbout",
                 RelatedFeedbackEvents = "relatedFeedbackEvents",
                  *)
                }

(* Jane Street version *)
(*
table feedback : {
      CreatedMoment : time,
      WorkdayId : wid,
      Subject : string, (* who the feedback is about *)
      Body : string, (* response *)
      Date : time,
      Request : string, (* who the feedback is about *)
      Guidance : string,
      Provider : string, (* who writes the feedback *)
      RequestedBy : string,
      RequestedDate : time,
      Status : string,
      SendMail : bool,
      LastFunctionallyUpdated : time
} PRIMARY KEY WorkdayId

type feedbackDisplay = {
      CreatedMoment : time,
      About : string,
      From : string,
      RequestedBy : string,
      Comments : string,
      LastFunctionallyUpdated : time
}
*)
(* my version *)
datatype feedbackStatus = Complete | Requested
(*
val show_feedbackStatus : show feedbackStatus =
 fn s => case s of
             Complete => "Complete"
           | Requested => "Requested"
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
      Status = string, (* TODO: use feedbackStatus *)
      SendMail = bool,
      LastUpdated = time
]

table workers : worker
  PRIMARY KEY Id
con workers_hidden_constraints = []
constraint [Pkey = [Id]] ~ workers_hidden_constraints

table feedback : feedback
  PRIMARY KEY Id,
  CONSTRAINT About FOREIGN KEY About REFERENCES workers(Id),
  CONSTRAINT Provider FOREIGN KEY Provider REFERENCES workers(Id),
  CONSTRAINT RequestedBy FOREIGN KEY RequestedBy REFERENCES workers(Id)
con feedback_hidden_constraints = []
(*constraint ([Pkey = [Id]] ++ [About = [], Provider = [], RequestedBy = []]) ~
 feedback_hidden_constraints*)

table directReports : {
      Manager : wid,
      Report : wid
} CONSTRAINT Manager FOREIGN KEY Manager REFERENCES workers(Id),
  CONSTRAINT Report FOREIGN KEY Report REFERENCES workers(Id)
con directReports_hidden_constraints = []
constraint [Manager, Report] ~ directReports_hidden_constraints

(* TODO: url table? *)

(* The user-facing version of feedback requests.
   This is what workers see when they check their outstanding feedback
   solicitations.
 *)
type feedbackRequestDisplay = {
     Id : wid, (* hidden in UI *)
     RequestedDate : option time,
     About : string,
     Provider : wid,
     RequestedBy : wid,
     Body : string, (* response body *)
     Status : feedbackStatus
}

fun toWorkdayFeedback (f : $feedback) : anytimeFeedback =
    error <xml>TODO</xml>


(* ************************************************************************** *)

functor Make(M : AUTH) = struct
    open M

    (*
     desired features:
      * allow manager to request feedback of their subordinates on each other
      * notify the subordinate of this request (by email)
      * allow the subordinate to submit the feedback
      * notify the manager when feedback is submitted
      * allow manager to see the feedbacks they've requested
      * allow the subordinate to see the feedback they've been assigned
     *)

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Workday to use this feature.</xml>
          | Some tok => return tok

    val prefixCommon = "https://wcpdev-services1.wd101.myworkday.com/ccx/api/v1/janestreet_wcpdev1"
    val prefixPerformanceEnablement = "https://wcpdev-services1.wd101.myworkday.com/ccx/api/performanceEnablement/v5/janestreet_wcpdev1"
    datatype service = Common | PerformanceEnablement
    fun urlOfService service =
        case service of
          Common => prefixCommon
        | PerformanceEnablement => prefixPerformanceEnablement

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Workday response: " ^ show v);
        return v

    fun api service url =
        let val prefix = urlOfService service in
          tok <- token;
          debug ("Workday GET: " ^ prefix ^ url);
          logged (WorldFfi.get (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)
        end

    fun apiOpt service url =
        let val prefix = urlOfService service in
          tok <- token;
          debug ("Workday GET: " ^ prefix ^ url);
          logged (WorldFfi.getOpt (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)
        end

    fun apiPost service url body =
        let val prefix = urlOfService service in
          tok <- token;
          debug ("Workday POST: " ^ prefix ^ url);
          debug ("Workday body: " ^ body);
          logged (WorldFfi.post (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/json") body)
        end

    (* High-level interface for interacting with local Nectry tables. *)
    structure Workers = struct
        val list : transaction (list $worker) =
            queryL1 (SELECT * FROM workers)

        fun manager (workerId : wid) : transaction $worker =
            res <- oneRow (SELECT workers.Id, workers.WName, workers.IsManager,
                      workers.PrimaryWorkEmail
                    FROM directReports JOIN workers
                    ON workers.Id = directReports.Manager
                    WHERE directReports.Report = {[workerId]});
            return (res.Workers)


        fun reports (managerId : wid) : transaction (list $worker) =
            res <- queryL (SELECT workers.Id, workers.WName, workers.IsManager,
                    workers.PrimaryWorkEmail FROM directReports JOIN workers
                     ON workers.Id = directReports.Report
                     WHERE directReports.Manager = {[managerId]});
            return (List.mp (fn r => r.Workers) res)

        (* TODO: historical relationship *)

        (* TODO:
         * list feedback you've requested
         * list feedback assigned to you
         * get guidance text
         * "solicit feedback" page
         * present page of feedback submission form ("provide feedback")
         * only present the page (url has the request ID) if the "provider" matches the whoami
         *)
    end

    (* Interface to Workday API for making GET and POST requests. *)
    structure WorkdayApi = struct
        structure WorkersApi = struct
            fun get (workerId : wid) : transaction $worker =
                s <- api Common ("/workers/" ^ workerId);
                return (fromJson s : response $worker).Data
                (* TODO: maybe this should return option worker? *)

            val getAll : transaction (list $worker) =
                s <- api Common "/workers";
                return (fromJson s : response (list $worker)).Data
        end

        structure FeedbackApi = struct
            fun get (workerId : wid) : transaction anytimeFeedback =
                s <- api PerformanceEnablement ("/workers/" ^ workerId ^ "/anytimeFeedback");
                return (fromJson s : response anytimeFeedback).Data

            val getAll : transaction (list anytimeFeedback) =
                workers <- Workers.list;
                List.mapM (fn w => get w.Id) workers

            fun post (response : anytimeFeedback) : transaction unit =
                resp <- apiPost PerformanceEnablement ("/workers/" ^ response.ToWorker.Id) (toJson response);
                return ()
        end

        structure DirectReportsApi = struct
            fun get (managerId : wid) : transaction directReports =
                s <- api Common ("/workers/" ^ managerId ^ "/directReports");
                reports <- return (fromJson s : list $worker);
                return {Manager = managerId,
                        Reports = List.mp (fn w => w.Id) reports}
        end
    end

    (* High-level interface for interacting with local Nectry tables. *)
    structure Feedback = struct
        val list : transaction (list $feedback) =
            queryL1 (SELECT * FROM feedback)
            (* TODO: whoami *)

        (* TODO: distinguish between anytime and solicited feedback lists *)
        (* TODO: could defunctionalize this by passing a set of flags:
           * Assigned | Submitted | Anytime | All
           * Incomplete (aka Outstanding) | Completed | All
         *)
        val assigned : transaction (list $feedback) =
            queryL1 (SELECT * FROM feedback
                     WHERE feedback.Status = "Requested")

        (* Requests you have submitted, regardless of status. Empty for non-managers. *)
        val submitted : transaction (list $feedback) =
            queryL1 (SELECT * FROM feedback
                     WHERE feedback.Status = "Complete")

        fun submit (response : $feedback) : transaction unit =
            WorkdayApi.FeedbackApi.post (toWorkdayFeedback response)

        fun request (request : $feedback) : transaction unit =
            (* TODO distinguish between anytime and solicited responses *)
            error <xml>unimplemented</xml>

        (* TODO: use this convenient function from Zoom for getting data from within a certain date range? *)
        (*
        val listPast =
            now <- now;
            let
                fun daysEarlier days = timef "%Y-%m-%d" (addSeconds now (days * (-24) * 60 * 60))

                fun grabChunk daysInPast acc =
                    if daysInPast > 30 * 6 then
                        return acc
                    else
                        ms <- apiPaged "meetings" ("metrics/meetings?type=past&from=" ^ daysEarlier daysInPast
                                                   ^ "&to=" ^ daysEarlier (daysInPast - 30));
                        grabChunk (daysInPast + 30) (List.append (List.mp pastMeetingIn ms) acc)
            in
                grabChunk 30 []
            end
            *)
    end


end