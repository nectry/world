open Json

signature AUTH = sig
    val token : transaction (option string)
end

type instance = string
val read_instance = _
val show_instance = _

(* Opaque type for all Workday IDs. *)
type wid = string
val show_wid = _
val ord_wid = mkOrd {Lt = fn a b => (readError a : int) < readError b,
                     Le = fn a b => (readError a : int) <= readError b}
val inj_wid = _

type worker = {
     Id : wid,
     WName : string, (* name *)
     IsManager : bool,
     PrimaryWorkEmail : option string
}
val _ : json worker = json_record_withOptional
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
type response object = {Data : object, Total : int}
fun json_response [object] (_ : json object) : json (response object) =
    json_record {Data = "data", Total = "total"}

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

type feedback = {
      Wid : option wid,
      Created : time,
      RequestedDate : option time,
      About : wid, (* who the feedback is about *)
      Provider : wid, (* who writes the feedback *)
      RequestedBy : option wid,
      Body : string, (* response *)
      Guidance : string,
      Status : string, (* TODO: use feedbackStatus *)
      SendMail : bool,
      LastUpdated : time
}

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Workday to use this feature.</xml>
          | Some tok => return tok

    datatype service = Common | PerformanceEnablement | Soap
    fun prefix service inst =
        let val suffix =
          case service of
            Common => "api/v1/"
          | PerformanceEnablement => "api/performanceEnablement/v5/"
          | Soap => "service/"
        in "https://" ^ "wcpdev-services1.wd101.myworkday.com/" ^ "ccx/" ^ suffix ^ inst
        end

    fun logged [a] (_ : show a) (t : transaction a) =
        v <- t;
        debug ("Workday response: " ^ show v);
        return v

    fun api url =
        tok <- token;
        debug ("Workday GET: " ^ url);
        logged (WorldFfi.get (bless url) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)

    fun apiOpt url =
        tok <- token;
        debug ("Workday GET: " ^ url);
        logged (WorldFfi.getOpt (bless url) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)

    fun apiPost url body =
        tok <- token;
        debug ("Workday POST: " ^ url);
        debug ("Workday body: " ^ body);
        logged (WorldFfi.post (bless url) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/json") body)

    fun depaginate [object] (j : json object) (limit : int) (service : service) (inst : instance) (endpoint : string) : transaction (list object) =
        let fun depaginate' (offset : int) =
          jsonResponse <- api ((prefix service inst) ^ endpoint ^ "?limit=" ^ (show limit) ^ "&offset=" ^ (show offset));
          let val resp = (fromJson jsonResponse : response (list object))
          in
            if offset >= resp.Total
            then return resp.Data
            else
                rest <- depaginate' (offset + limit);
                return (List.append resp.Data rest)
          end
        in
          depaginate' 0
        end

    structure Workers = struct
        fun get inst (workerId : wid) : transaction worker =
            s <- api ((prefix Common inst) ^ "/workers/" ^ Urls.urlencode workerId);
            return (fromJson s : response worker).Data
            (* TODO: maybe this should return option worker? *)

        fun getAll inst : transaction (list worker) =
            depaginate 100 Common inst "/workers"
    end

    structure Feedback = struct
        fun get inst (workerId : wid) : transaction (list anytimeFeedback) =
            depaginate 100 PerformanceEnablement inst ("/workers/" ^ Urls.urlencode workerId ^ "/anytimeFeedbackEvents")

        fun getAll inst (workers : list wid) : transaction (list anytimeFeedback) =
            List.mapConcatM (get inst) workers

        fun parseSoap (xmlString : string) : wid =
            (* HACK: actually parse XML *)
            case String.ssplit {Haystack = xmlString, Needle = "<wd:ID wd:type=\"WID\">"} of
                None => error <xml>Could not find ID in response: {[xmlString]}</xml>
              | Some (pre, post) =>
                case String.split post #"<" of
                    None => error <xml>Badly formed response: {[xmlString]}</xml>
                  | Some (pre, post) => pre
        fun post inst (request : feedback) : transaction wid =
            (* TODO: patch case where Feedback.Id is `Some i` *)
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
            <bsvc:Comment>" ^ (case show request.Body of "" => "[Awaiting Feedback From Provider]" | s => s) ^ "</bsvc:Comment>
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
              url <- return ((prefix Soap inst) ^ "/Talent/v42.2");
              resp <- logged (WorldFfi.post (bless url) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/xml; charset=utf-8") body);
              return (parseSoap resp)
            end
    end

    structure DirectReports = struct
        fun get inst (managerId : wid) : transaction directReports =
            s <- api ((prefix Common inst) ^ "/workers/" ^ Urls.urlencode managerId ^ "/directReports?limit=100"); (* HACK: proper depagination; because maybe someone has over 100 direct reports *)
            reports <- return (fromJson s : response (list worker)).Data;
            return {Manager = managerId,
                    Reports = (*List.filter
                                  (fn i => List.mem i workers)*)
                                  (List.mp (fn w => w.Id) reports)}
    end
end
