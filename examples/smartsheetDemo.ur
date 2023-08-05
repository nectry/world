(* For this demo, it's necessary to create smartsheetSecrets.ur,
 * defining [api_token]. *)
structure Z = Smartsheet.Make(Smartsheet.TwoLegged(SmartsheetSecrets))

fun create tid wid r =
    sid <- Z.Sheets.createInWorkspace wid ({Nam = r.Nam} ++ Api.optionals {FromId = tid});
    return <xml><body>
      I created sheet #{[sid]}.
    </body></xml>

fun workspace tid wid =
    return <xml><body>
      <form>
	New sheet name: <textbox{#Nam}/><br/>
	<submit action={create tid wid}/>
      </form>
    </body></xml>

fun template tid =
    ws <- Z.Workspaces.list;
    return <xml><body>
      <h2>Workspaces</h2>
      <ul>
        {List.mapX (fn r =>
		       case r.Id of
			   None => error <xml>No ID returned for workspace</xml>
			 | Some wid => <xml><li><a link={workspace tid wid}>{[r.Nam]}</a></li></xml>) ws}
      </ul>
    </body></xml>

val main =
    ts <- Z.Templates.list;
    return <xml><body>
      <h2>Templates</h2>
      <ul>
        {List.mapX (fn r =>
		       case r.Id of
			   None => error <xml>No ID returned for template</xml>
			 | Some tid => <xml><li><a link={template tid}>{[r.Nam]}</a></li></xml>) ts}
      </ul>
    </body></xml>
