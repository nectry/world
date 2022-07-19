open Json

structure Scope = struct
    type t = Scopes.t [ChannelsHistory, ChannelsManage, ChannelsWrite, ChannelsRead,
                       IdentityBasic, IdentityEmail]
    val empty = Scopes.empty
    val union = Scopes.union
    val toString = Scopes.toString {ChannelsHistory = "channels:history",
                                    ChannelsManage = "channels:manage",
                                    ChannelsWrite = "channels:write",
                                    ChannelsRead = "channels:read",
                                    IdentityBasic = "identity.basic",
                                    IdentityEmail = "identity.email"}

    val channelsHistory = Scopes.one [#ChannelsHistory]
    val channelsManage = Scopes.one [#ChannelsManage]
    val channelsRead = Scopes.one [#ChannelsRead]
    val channelsWrite = Scopes.one [#ChannelsWrite]
    val identityBasic = Scopes.one [#IdentityBasic]
    val identityEmail = Scopes.one [#IdentityEmail]

    val readonly = Scopes.disjoint (union channelsWrite channelsManage)
end

signature AUTH = sig
    val token : transaction (option string)
end

functor TwoLegged(M : sig
                      val token : string
                  end) = struct
    open M

    val token = return (Some token)
end

functor ThreeLegged(M : sig
                        val client_id : string
                        val client_secret : string
                        val https : bool

                        val scopes : Scope.t
                        val onCompletion : transaction page
                    end) = struct
    open M

    table secrets : { Secret : int,
                      Token : string,
                      Expires : time }
      PRIMARY KEY Secret

    task periodic 60 = fn () =>
                          tm <- now;
                          dml (DELETE FROM secrets
                               WHERE Expires < {[addSeconds tm (-60)]})

    cookie user : int

    fun withToken {Token = tok, Expiration = seconds, ...} =
        seconds <- return (Option.get (60 * 60) seconds);
        secret <- rand;
        tm <- now;
        dml (INSERT INTO secrets(Secret, Token, Expires)
             VALUES ({[secret]}, {[tok]}, {[addSeconds tm (seconds * 3 / 4)]}));
        setCookie user {Value = secret,
                        Expires = None,
                        Secure = https}

    open Oauth.Make(struct
                        open M

                        val authorize_url = bless "https://slack.com/oauth/v2/authorize"
                        val access_token_url = bless "https://slack.com/api/oauth.v2.access"

                        val withToken = withToken
                        val scope = Some (Scope.toString scopes)
                        val nameForScopeParameter = Some "user_scope"
                        val hosted_domain = None

                        type token_response = {
                             AuthedUser : { Token : string }
                        }
                        val _ : json {Token : string} = json_record {Token = "access_token"}
                        val _ : json token_response = json_record {AuthedUser = "authed_user"}
                        val parseTokenResponse = Some (fn s =>
                                                          (fromJson s : token_response).AuthedUser
                                                       ++ {Expires = None})
                    end)

    val token =
        c <- getCookie user;
        case c of
            None => return None
          | Some n =>
            oneOrNoRowsE1 (SELECT (secrets.Token)
                           FROM secrets
                           WHERE secrets.Secret = {[n]}
                             AND secrets.Expires > CURRENT_TIMESTAMP)

    val logout = clearCookie user

    val status =
        toko <- token;
        li <- source (Option.isSome toko);
        cur <- currentUrl;
        return <xml>
          <dyn signal={liV <- signal li;
                       if liV then
                           return <xml><button value="Log out of Slack"
                                               onclick={fn _ => rpc logout; set li False}/></xml>
                       else
                           return <xml><button value="Log into Slack"
                                               onclick={fn _ => redirect (url authorize)}/></xml>}/>
        </xml>
end

val _ : json time = json_derived (addSeconds minTime) toSeconds

type topic_or_purpose = {
     Value : string,
     Creator : string,
     LastSet : time
}
val _ : json topic_or_purpose = json_record {Value = "value",
                                             Creator = "creator",
                                             LastSet = "last_set"}

type conversation = {
     Id : string,
     Nam : string,
     IsChannel : bool,
     IsGroup : bool,
     IsIm : bool,
     Created : time,
     Creator : string,
     IsArchived : bool,
     IsGeneral : bool,
     Unlinked : int,
     NameNormalized : string,
     IsReadOnly : option bool,
     IsShared : bool,
     IsExtShared : bool,
     IsOrgShared : bool,
     SharedTeamIds : option (list string),
     PendingShared : list string,
     IsPendingExtShared : bool,
     IsMember : bool,
     IsPrivate : bool,
     IsMpim : bool,
     LastRead : option string,
     Topic : topic_or_purpose,
     Purpose : topic_or_purpose,
     PreviousNames : list string,
     NumMembers : option int,
     Locale : option string
}
val _ : json conversation = json_record_withOptional
                                {Id = "id",
                                 Nam = "name",
                                 IsChannel = "is_channel",
                                 IsGroup = "is_group",
                                 IsIm = "is_im",
                                 Created = "created",
                                 Creator = "creator",
                                 IsArchived = "is_archived",
                                 IsGeneral = "is_general",
                                 Unlinked = "unlinked",
                                 NameNormalized = "name_normalized",
                                 IsShared = "is_shared",
                                 IsExtShared = "is_ext_shared",
                                 IsOrgShared = "is_org_shared",
                                 PendingShared = "pending_shared",
                                 IsPendingExtShared = "is_pending_ext_shared",
                                 IsMember = "is_member",
                                 IsPrivate = "is_private",
                                 IsMpim = "is_mpim",
                                 Topic = "topic",
                                 Purpose = "purpose",
                                 PreviousNames = "previous_names"}
                                {IsReadOnly = "is_read_only",
                                 LastRead = "last_read",
                                 Locale = "locale",
                                 SharedTeamIds = "shared_team_ids",
                                 NumMembers = "num_members"}

type edited = {
     User : string,
     Ts : string
}
val _ : json edited = json_record {User = "user",
                                   Ts = "ts"}

type reaction = {
     Nam : string,
     Count : int,
     Users : list string
}
val _ : json reaction = json_record {Nam = "name",
                                     Count = "count",
                                     Users = "users"}

type message = {
     Typ : string,
     Subtype : option string,
     Channel : option string,
     User : string,
     Text : string,
     Ts : string,
     Edited : option edited,
     Hidden : option bool,
     IsStarred : option bool,
     PinnedTo : option (list string),
     Reactions : option (list reaction)
}
val _ : json message = json_record_withOptional
                           {Typ = "type",
                            User = "user",
                            Text = "text",
                            Ts = "ts"}
                           {Subtype = "subtype",
                            Channel = "channel",
                            Edited = "edited",
                            Hidden = "hidden",
                            IsStarred = "is_starred",
                            PinnedTo = "pinned_to",
                            Reactions = "reactions"}

type profile = {
     Title : string,
     Phone : string,
     Skype : string,
     RealName : string,
     RealNameNormalized : string,
     DisplayName : string,
     DisplayNameNormalized : string,
     StatusText : string,
     StatusEmoji : string,
     StatusExpiration : time,
     AvatarHash : string,
     FirstName : option string,
     LastName : option string,
     Email : option string,
     ImageOriginal : option string,
     Image24 : option string,
     Image32 : option string,
     Image48 : option string,
     Image72 : option string,
     Image192 : option string,
     Image512 : option string,
     Team : string
}
val _ : json profile = json_record_withOptional
                           {Title = "title",
                            Phone = "phone",
                            Skype = "skype",
                            RealName = "real_name",
                            RealNameNormalized = "real_name_normalized",
                            DisplayName = "display_name",
                            DisplayNameNormalized = "display_name_normalized",
                            StatusText = "status_text",
                            StatusEmoji = "status_emoji",
                            StatusExpiration = "status_expiration",
                            AvatarHash = "avatar_hash",
                            Team = "team"}
                           {Email = "email",
                            FirstName = "first_name",
                            LastName = "last_name",
                            ImageOriginal = "image_original",
                            Image24 = "image_24",
                            Image32 = "image_32",
                            Image48 = "image_48",
                            Image72 = "image_72",
                            Image192 = "image_192",
                            Image512 = "image_512"}

type user = {
     Id : string,
     TeamId : string,
     Nam : string,
     Deleted : bool,
     Color : string,
     RealName : string,
     Tz : string,
     TzLabel : string,
     TzOffset : int,
     Profile : profile,
     IsAdmin : bool,
     IsOwner : bool,
     IsPrimaryOwner : bool,
     IsRestricted : bool,
     IsUltraRestricted : bool,
     IsBot : bool,
     IsStranger : option bool,
     Updated : time,
     IsAppUser : bool,
     IsInvitedUser : option bool,
     Has2fa : option bool,
     Locale : option string
}
val _ : json user = json_record_withOptional
                        {Id = "id",
                         TeamId = "team_id",
                         Nam = "name",
                         Deleted = "deleted",
                         Color = "color",
                         RealName = "real_name",
                         Tz = "tz",
                         TzLabel = "tz_label",
                         TzOffset = "tz_offset",
                         Profile = "profile",
                         IsAdmin = "is_admin",
                         IsOwner = "is_owner",
                         IsPrimaryOwner = "is_primary_owner",
                         IsRestricted = "is_restricted",
                         IsUltraRestricted = "is_ultra_restricted",
                         IsBot = "is_bot",
                         Updated = "updated",
                         IsAppUser = "is_app_user"}
                        {Has2fa = "has_2fa",
                         IsInvitedUser = "is_invited_user",
                         IsStranger = "is_stranger",
                         Locale = "locale"}

type identity = {
     Nam : string,
     Email : option string
}
val _ : json identity = json_record_withOptional
                            {Nam = "name"}
                            {Email = "email"}

val urlPrefix = "https://slack.com/"

functor Make(M : AUTH) = struct
    open M

    val token =
        toko <- token;
        case toko of
            None => error <xml>You must be logged into Slack to use this feature.</xml>
          | Some tok => return tok

    val prefix = "https://slack.com/api/"

    type slack_response = {
         Ok : bool,
         Error : option string,
    }
    val _ : json slack_response = json_record_withOptional {Ok = "ok"}
                                                           {Error = "error"}

    fun wrap_errcheck (t : transaction string) =
        s <- t;
        debug ("Response: " ^ s);
        sr <- return (fromJson s : slack_response);
        if sr.Ok then
            return s
        else
            error <xml>Slack API error: {[sr.Error]}</xml>

    fun api url =
        tok <- token;
        wrap_errcheck (WorldFfi.get (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False)

    fun apiOpt url =
        tok <- token;
        WorldFfi.getOpt (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) False

    fun apiPost url =
        tok <- token;
        debug ("POST request: " ^ prefix ^ url);
        wrap_errcheck (WorldFfi.post (bless (prefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) None "")

    fun oneJsonField [t ::: Type] (_ : json t) (label : string) (s : string) : t =
        let
            val j : json {Value : t} =
                json_record {Value = label}
        in
            (@fromJson j s).Value
        end

    fun apiField [t ::: Type] (_ : json t) (label : string) (url : string) : transaction t =
        page <- api url;
        return (oneJsonField label page)

    fun apiPostField [t ::: Type] (_ : json t) (label : string) (url : string) : transaction t =
        page <- apiPost url;
        return (oneJsonField label page)

    fun apiList [t ::: Type] (_ : json t) (listLabel : string) (url : string) : transaction (list t) =
        apiField listLabel url

    structure Conversations = struct
        val list = apiList "channels" "conversations.list"

        fun history ch = apiList "messages" ("conversations.history?channel=" ^ Urls.urlencode ch)

        fun create name =
            apiPostField "channel" ("conversations.create?name=" ^ Urls.urlencode name)

        fun setTopic id topic =
            Monad.ignore (apiPost ("conversations.setTopic?channel=" ^ Urls.urlencode id ^ "&topic=" ^ Urls.urlencode topic))

        fun setPurpose id purpose =
            Monad.ignore (apiPost ("conversations.setPurpose?channel=" ^ Urls.urlencode id ^ "&purpose=" ^ Urls.urlencode purpose))
    end

    structure Chat = struct
        fun postMessage r =
            apiPostField "message" ("chat.postMessage?channel=" ^ Urls.urlencode r.Channel
                                    ^ "&text=" ^ Urls.urlencode r.Text)
    end

    structure Users = struct
        fun info uid = apiField "user" ("users.info?user=" ^ Urls.urlencode uid)
        val identity =
            toko <- M.token;
            case toko of
                None => return None
              | Some _ => v <- apiField "user" "users.identity"; return (Some v)
    end
end

fun suggestChannelName s =
    let
        val len = String.length s

        fun build i acc =
            if i >= len || String.length acc >= 80 then
                acc
            else
                let
                    val ch = String.sub s i
                    val cho =
                        if Char.isAlnum ch then
                            Some (Char.toLower ch)
                        else if Char.isSpace ch then
                            Some #"-"
                        else if ch = #"-" || ch = #"_" then
                            Some ch
                        else
                            None
                in
                    build (i + 1) (case cho of
                                       None => acc
                                     | Some ch' => acc ^ String.str ch')
                end
    in
        build 0 ""
    end

fun channelUrl c = bless (urlPrefix ^ "app_redirect?channel=" ^ Urls.urlencode c.Channel
                          ^ case c.Team of
                                Some tid => "&team=" ^ Urls.urlencode tid
                              | _ => "")
