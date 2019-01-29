program Github_GraphQL_Demo;

uses
  Vcl.Forms,
  Form.Main in 'Form.Main.pas' {Form1},
  GraphQL.Base in '..\src\GraphQL.Base.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
