open Json
(* open Bootstrap *)

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
val _ : json $worker = json_record_withOptional
                           (* TODO *)
                      {Id = "id",
                       WName = "descriptor", (* name *)
                       IsManager = "isManager"}
                      {PrimaryWorkEmail = "primaryWorkEmail"}

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
     Descriptor : option string
}
val _ : json descriptor =
    json_record_withOptional
        {Id = "id"}
        {Descriptor = "descriptor"}

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
     For : option descriptor, (* could be another business process if this is a subprocess *)
     TransactionStatus : option descriptor,
     Action : option descriptor,
     OverallBusinessProcess : option descriptor,
     Comment : option string,
     Comments : option (list comment),
     WarningValidations : option string,
     Attachments : option (list attachment),
     OverallStatus : option string,
     CriticalValidations : option string,
}
val _ : json businessProcessParameters =
    json_record_withOptional
        {}
        {For = "for",
         Action = "action",
         TransactionStatus = "transactionStatus",
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
val json_anytimeFeedback : json anytimeFeedback =
    json_record_withOptional
        {Id = "id"}
        {Preview = "descriptor",
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

con feedbackWithoutId = [
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
            fun get (workerId : wid) : transaction (list anytimeFeedback) =
                s <- api PerformanceEnablement ("/workers/" ^ workerId ^ "/anytimeFeedbackEvents");
                workers <- Monad.mp (List.mp (fn w => w.Id)) Workers.list;
                return (
                  (* Filter out feedback where the provider isn't in the workers table. *)
                  (* HACK *)
                  (* TODO: can we guarantee this doesn't happen? *)
                  List.filter
                    (fn fb => List.mem (Option.getOrError <xml>no</xml> fb.FromWorker).Id workers)
                    (fromJson s : response (list anytimeFeedback)).Data)

            val getAll : transaction (list anytimeFeedback) =
                workers <- Workers.list;
                List.mapConcatM (fn w => get w.Id) workers

            fun parseSoap (xmlString : string) : wid =
                (* HACK: actually parse XML *)
                case String.ssplit {Haystack = xmlString, Needle = "<wd:ID wd:type=\"WID\">"} of
                    None => error <xml>Could not find ID in response: {[xmlString]}</xml>
                  | Some (pre, post) =>
                    case String.split post #"<" of
                        None => error <xml>Badly formed response: {[xmlString]}</xml>
                      | Some (pre, post) => pre
            fun post (request : $feedbackWithoutId) : transaction wid =
                let fun toSoapXml request =
                        "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<env:Envelope xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\">
    <env:Body>
        <bsvc:Give_Feedback_Request xmlns:bsvc=\"urn:com.workday/bsvc\" bsvc:version=\"v42.2\">
            <bsvc:Give_Feedback_Data>
                <bsvc:From_Worker_Reference>
                    <bsvc:ID bsvc:type=\"WID\">" ^ request.Provider ^ "</bsvc:ID>
                </bsvc:From_Worker_Reference>
                <bsvc:To_Workers_Reference>
                    <bsvc:ID bsvc:type=\"WID\">" ^ request.About ^ "</bsvc:ID>
                </bsvc:To_Workers_Reference>
                <bsvc:Comment>" ^ request.Body ^ "</bsvc:Comment>
                <bsvc:Show_Name>true</bsvc:Show_Name>
                <bsvc:Confidential>true</bsvc:Confidential>
            </bsvc:Give_Feedback_Data>
        </bsvc:Give_Feedback_Request>
    </env:Body>
</env:Envelope>"
        (* TODO: extra information: status, dates, requestedBy, etc *)

                in
                  body <- return (toSoapXml request);
                  tok <- token;
                  debug tok;
                  url <- return "https://wcpdev-services1.wd101.myworkday.com/ccx/service/janestreet_wcpdev1/Talent/v42.2";
                  resp <- logged (WorldFfi.post (bless url) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/xml; charset=utf-8") body);
                  return (parseSoap resp)
                end
        end

        structure DirectReportsApi = struct
            fun get (managerId : wid) : transaction directReports =
                s <- api Common ("/workers/" ^ managerId ^ "/directReports");
                reports <- return (fromJson s : response (list $worker)).Data;
                workers <- Monad.mp (List.mp (fn w => w.Id)) Workers.list;
                return {Manager = managerId,
                        Reports = List.filter
                                      (fn i => List.mem i workers)
                                      (List.mp (fn w => w.Id) reports)}
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

        (*
        fun submit (response : $feedback) : transaction unit =
            WorkdayApi.FeedbackApi.post (toWorkdayFeedback response)

        fun request (request : $feedback) : transaction unit =
            (* TODO distinguish between anytime and solicited responses *)
            error <xml>unimplemented</xml>
        *)

        fun post (obj : $feedbackWithoutId) : transaction wid =
            WorkdayApi.FeedbackApi.post obj
            (*i <- rpc rand;
            return (show i)*)


        fun patch () : transaction unit =
            (* TODO: where in Give Feedback SOAP request do we put the ID? *)
            (* _ <- WorkdayApi.FeedbackApi.post (toWorkdayFeedback obj); *)
            return ()

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
