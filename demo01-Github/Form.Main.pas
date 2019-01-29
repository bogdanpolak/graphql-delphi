unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Actions,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdCtrls,
  GraphQL.Base;

type
  TForm1 = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    ActionList1: TActionList;
    actQueryUser: TAction;
    actQueryRepo: TAction;
    Action3: TAction;
    Memo1: TMemo;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure actQueryUserExecute(Sender: TObject);
    procedure actQueryRepoExecute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
  private
    GraphQL: TGraphQL;
    procedure DoCallGitHubAPI(const GraphQLQuery: string;
      GraphQLVariables: string = '');
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  System.JSON;

{$INCLUDE SecureConsts.inc}
const
  // ------------------------------------------------------------------------
  // README:
  //
  // * GitHub account is required to run this demo
  // * Remove $INCLUDE SecureConsts.inc
  // * Enter your personal access token
  //
  // More info:
  // https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/
  //
  GITHUB_AccessToken = GitHub_Secure_AccessToken;

procedure TForm1.DoCallGitHubAPI(const GraphQLQuery: string;
  GraphQLVariables: string = '');
var
  jsVariables: TJSONObject;
  AResponse: string;
begin
  GraphQL.URL := 'https://api.github.com/graphql';
  GraphQL.Authorization := 'bearer ' + GITHUB_AccessToken;
  if GraphQLVariables = '' then
    AResponse := GraphQL.CallQuery(GraphQLQuery)
  else
  begin
    jsVariables := TJSONObject.ParseJSONValue(GraphQLVariables) as TJSONObject;
    AResponse := GraphQL.CallQuery(GraphQLQuery, jsVariables);
    jsVariables.Free;
  end;
  // ---
  Memo1.Lines.Add('----------------------');
  Memo1.Lines.Add(AResponse);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  GraphQL := TGraphQL.Create(Self);
  Memo1.Align := alClient;
  Memo1.Clear;
end;

procedure TForm1.actQueryUserExecute(Sender: TObject);
var
  Query: string;
begin
  Query := 'query { viewer { login } }';
  DoCallGitHubAPI(Query);
end;

procedure TForm1.actQueryRepoExecute(Sender: TObject);
var
  Query: string;
  Variables: string;
begin
  Query := 'query ($maxRepos:Int!) { ' + sGraphNewLine +
    'viewer { name repositories(last: $maxRepos) { nodes {name} } } ' +
    sGraphNewLine + ' }';
  Variables := '{"maxRepos":12}';
  DoCallGitHubAPI(Query, Variables);
end;

procedure TForm1.Action3Execute(Sender: TObject);
begin
  // x
end;

end.
