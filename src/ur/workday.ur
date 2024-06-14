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

(* Opaque type for all Workday IDs. *)
type wid = string

type worker = {
     Id : wid,
     WName : string, (* name *)
     IsManager : bool,
     PrimaryWorkEmail : string
}
val _ : json worker = json_record
                      {Id = "id",
                       WName = "descriptor", (* name *)
                       IsManager = "isManager",
                       PrimaryWorkEmail = "primaryWorkEmail"}

type directReports = {
     Id : wid, (* manager *)
     Reports : list wid (* list of worker ids *)
}

(* could be anonymous instance by inlining the record structure *)
type response object = {Data : object}
fun json_response [object] (_ : json object) : json (response object) =
    json_record {Data : "data"}

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
type feedbackStatus = Complete | Requested
(* TODO: typeclasses like eq and sql_injectable *)

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
table workers : {
      Id : wid,
      WName : string, (* name *)
      IsManager : bool,
      PrimaryWorkEmail : string
} PRIMARY KEY Id
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
        let prefix = urlOfService service in
          tok <- token;
          debug ("Workday GET: " ^ prefix ^ url);
          logged (WorldFfi.get (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)
        end

    fun apiOpt service url =
        let prefix = urlOfService service in
          tok <- token;
          debug ("Workday GET: " ^ prefix ^ url);
          logged (WorldFfi.getOpt (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)
        end

    fun apiPost service url body =
        let prefix = urlOfService service in
          tok <- token;
          debug ("Workday POST: " ^ prefix ^ url);
          debug ("Workday body: " ^ body);
          logged (WorldFfi.post (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/json") body)
        end

    (* Interface to Workday API for making GET and POST requests. *)
    structure WorkdayApi = struct
        structure Workers = struct
            fun get (workerId : wid) : transaction worker =
                s <- api Common ("/workers/" ^ workerId);
                return (fromJson s).Data
                (* TODO: maybe this should return option worker? *)

            val getAll : transaction (list worker) =
                s <- api Common "/workers";
                return (fromJson s).Data
        end

        structure Feedback = struct
            fun get (workerId : wid) : transaction (list anytimeFeedback) =
                s <- api PerformanceEnablement "/workers/" ^ workerId ^ "/anytimeFeedback";
                return (fromJson s).Data

            val getAll : transaction (list anytimeFeedback)
                workers <- Workers.list;
                List.mapConcatM (fn w => WorkdayAPI.Feedback.get w.Id) workers

            (* TODO: post *)
        end

        structure DirectReports = struct
            fun get (managerId : wid) : directReports =
                s <- api Common ("/workers/" ^ managerId ^ "/directReports");
                reports <- return (fromJson s : list worker);
                return {Id = managerId,
                        Reports = List.mp (fn w => w.Id) reports}
        end
    end

    (* High-level interface for interacting with local Nectry tables. *)
    structure Workers = struct
        val list : transaction (list worker) =
            queryL1 (SELECT * FROM workers);

        fun manager (workerId : wid) : transaction worker =
            oneRow (SELECT t.Manager FROM directReports AS t
                    WHERE t.Report = {workerId});

        fun reports (managerId : wid) : transaction (list worker) =
            queryL1 (SELECT t.Report FROM directReports AS t
                     WHERE t.Manager = {managerId});

        (* TODO:
         * list feedback you've requested
         * list feedback assigned to you
         * get guidance text
         * "solicit feedback" page
         * present page of feedback submission form ("provide feedback")
         * only present the page (url has the request ID) if the "provider" matches the whoami
         *)

    (* High-level interface for interacting with local Nectry tables. *)
    structure Feedback = struct
        val list : transaction (list feedbackRequestDisplay) =
            (* TODO *)
            (* whoami *)

        (* TODO: could defunctionalize this by passing a set of flags:
           * Assigned | Submitted | Anytime | All
           * Incomplete (aka Outstanding) | Completed | All
         *)
        val assignedRequests : transaction (list feedback) =
            (* TODO *)

        (* Requests you have submitted, regardless of status. Empty for non-managers. *)
        val submittedRequests : transaction (list feedback) =
            (* TODO *)

        (* Requests you have completed *)
        val completedRequests : transaction (list feedback) =
            (* TODO *)

        fun submit (response : feedback) : transaction unit =
            (* TODO *)

        fun request (request : feedback) : transaction unit =
            (* TODO *)

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

(* Functor for deep link handler. *)
(* Takes url as input. *)
functor MakeForm(M : sig
                     (* TODO: arguments to complex form *)
                     (* TODO: need table argument *)
  (*
  con tabColumns :: {Type}
  con ingredientConfig :: Type
  con ingredientState :: Type

  val rowIngredients : form [] tabColumns ingredientConfig ingredientState
  val labels : $(map (fn _ => string) tabColumns)

  val fl : folder tabColumns
  val inj : $(map sql_injectable tabColumns)

  val buttonText : string

  val whoami : transaction (option string)

  table tab : $tabColumns
  val title : string
  *)

                 end) = struct
  (* TODO: look up the URL in the table? *)
  (* create the complex form UI *)

    type input = wid (* or string? *)

    formUi = ComplexForm.MakeForm(M) (* non-modal form *)

    (* turn S0 into S *)
    (* takes row ID as input *)
    val ui = {Render = formUi.Render,
              Create = formUi.Create,
              formUi.
    end


functor MakeSync(M : sig
                 include NectryApi.SYNC_IN

                 table directReports : {Id : wid,
                                        Reports : list wid}
                 table worker : {Id : wid,
                                 WName : string, (* name *)
                                 IsManager : bool,
                                 PrimaryWorkEmail : string}
                 table feedback : {Id : wid,
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
                                   LastUpdated : time}
             end) = struct
    open M

    structure W = Make(M) (* main module Make - i.e. give access to API calls *)

    val n_sync =
        (* Pull worker data. *)
        workers <- W.WorkdayApi.Workers.getAll;
        List.app (fn w : worker =>
                  (alreadyThere <- oneRowE1 (SELECT COUNT( * ) > 0
                                            FROM workers
                                            WHERE workers.N_Workday_0020Worker_Id = {[w.Id]});
                   (if alreadyThere then
                         return ()
                     else
                         dml (INSERT INTO workers(
                               N_Workday_0020Worker_Id,
                               N_Workday_0020Worker_Descriptor,
                               N_Workday_0020Worker_IsManager,
                               N_Workday_0020Worker_Email)
                             VALUES ({[w.Id]}, {[w.Descriptor]}, {[w.IsManager]},
                                     {[m.Email]})));
                 )) workers;

        ChangeWatcher.changed "Worker";

        (* Compute direct reports. *)
        directReports <- List.map (fn m => {Manager = m.Id,
                                            Reports = W.WorkdayApi.DirectReports.get m.Id})
               (filter (fn w : worker => w.IsManager) workers);
        List.app (fn dr =>
                  List.app (fn rep =>
                    (alreadyThere <- oneRowE1 (SELECT COUNT ( * ) > 0
                                              FROM directReports AS drs
                                              WHERE drs.Manager = {[dr.Manager]}
                                              AND drs.Report = {[rep]});
                    (if alreadyThere then
                          return ()
                     else
                          dml (INSERT INTO directReports(Manager, Report)
                               VALUES ({[dr.Manager]}, {[rep]})));
                     )) dr.Reports)
                 directReports;

        ChangeWatcher.changed "Direct Reports";

        (* Pull feedback data. *)
        feedback <- W.WorkdayApi.Feedback.getAll;
        List.app (fn f : feedback =>
                  (alreadyThere <- oneRowE1 (SELECT COUNT( * ) > 0
                                            FROM feedback
                                            WHERE feedback.Id = {[f.Id]});
                   (if alreadyThere then
                         return ()
                     else
                         dml (INSERT INTO feedback(
                               Id,
                               Url,
                               Created,
                               RequestedDate,
                               About,
                               Provider,
                               RequestedBy,
                               Body,
                               Guidance,
                               Status,
                               SendMail,
                               LastUpdated)
                             VALUES ({[f.Id]}, {[f.Url]}, {[f.Created]},
                                     {[f.RequestedDate]},
                                     {[f.About]},
                                     {[f.Provider]},
                                     {[f.RequestedBy]},
                                     {[f.Body]},
                                     {[f.Guidance]},
                                     {[f.Status]},
                                     {[f.SendMail]},
                                     {[f.LastUpdated]})));
                 )) feedback;

        ChangeWatcher.changed "Feedback"

    (* TODO: will we use webhooks? *)
    fun n_webhook pbody =
        return ()
end

type token_response = {
     AccessToken : string,
     RefreshToken : string
}
val _ : json token_response =
    json_record {AccessToken = "access_token",
                 RefreshToken = "refresh_token"}
(* TODO: get expiration time from response? *)

functor TwoLegged(M : sig
                      val client_id : string
                      val client_secret : string
                  end) = struct
    open M

    table mytoken : { Token : string,
                      Expires : time,
                      Refresh : string }

    task periodic 60 = fn () =>
                          tm <- now;
                          dml (DELETE FROM mytoken
                               WHERE Expires < {[addSeconds tm (-60)]})

    (* Given a refresh token, generate a new token. *)
    fun refresh refreshToken =
        let val basic = Urls.base64url_encode (client_id ^ ":" ^ client_secret)
            val headers = WorldFfi.addHeader WorldFfi.emptyHeaders
                                             "Authorization"
                                             ("Basic " ^ basic)
            val body = "grant_type=refresh_token&refresh_token=" ^ refreshToken
        in
            tm <- now;
            exp <- return (addSeconds tm (60 * 60 * 4));
            resp <- WorldFfi.post
                        (bless "https://wcpdev-services1.wd101.myworkday.com/ccx/oauth2/janestreet_wcpdev1/token")
                        headers
                        (Some "application/x-www-form-urlencoded")
                        body;
            resp <- return (fromJson resp : token_response);
            dml (INSERT INTO mytoken(Token, Expires, Refresh)
                 VALUES ({[resp.AccessToken]}, {[exp]}, {[resp.RefreshToken]}));
            return (Some resp.AccessToken);

    val token =
        tokopt <- oneOrNoRowsE1 (SELECT (mytoken.Token)
                                 FROM mytoken);
        case tokopt of
            Some tok =>
            tm <- now;
            exp <- tok.Expires;
            if exp < addSeconds tm (-60)
            then
                dml (DELETE FROM mytoken WHERE TRUE);
                refresh tok.Refresh;
            else return (Some tok)
          | None =>
            tm <- now;
            exp <- return (addSeconds tm (4 * 60 * 60));
            (* FIXME: how to get initial token??? *)

            basic <- return (Urls.base64url_encode (client_id ^ ":" ^ client_secret))
            headers <- return (WorldFfi.addHeader WorldFfi.emptyHeaders
                                                  "Authorization"
                                                  ("Basic " ^ basic))
            resp <- WorldFfi.post
                      (bless "https://wcpdev-services1.wd101.myworkday.com/ccx/oauth2/janestreet_wcpdev1/token")
                      headers
                      (Some "application/x-www-form-urlencoded");
            resp <- return (fromJson resp : token_response);
            dml (DELETE FROM mytoken WHERE TRUE);
            dml (INSERT INTO mytoken(Token, Expires, Refresh)
                 VALUES ({[resp.AccessToken]}, {[exp]}, {[resp.RefreshToken]}));
            return (Some token)
end

functor MakeTwoLegged(M : sig
                          val api_key : string
                          val api_secret : string
                          val scope : scope
                      end) = struct
    structure Z = TwoLegged(M)

    val token = Z.token
    val n_token = token
    val status = return <xml></xml>
    val n_status = status

    val n_auth = {Token = token,
                  Status = status,
                  Logout = return ()}
end
