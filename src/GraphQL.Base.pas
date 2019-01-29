unit GraphQL.Base;

interface

uses
  System.Classes,
  System.JSON;

const
  sGraphNewLine = '\n';

type
  TGraphQL = class(TComponent)
  private
    FURL: string;
    FAuthorization: string;
    procedure SetURL(const Value: string);
    procedure SetAuthorization(const Value: string);
  public
    function CallQuery(const Query: string; Variables: TJSONObject = nil)
      : string; overload;
    function CallQuery(const Query: string; const Variables: String)
      : string; overload;
    property URL: string read FURL write SetURL;
    property Authorization: string read FAuthorization write SetAuthorization;
  end;

implementation

uses
  System.SysUtils,
  System.NetConsts,
  System.Net.HttpClient,
  System.Net.URLClient;

function TGraphQL.CallQuery(const Query: string;
  Variables: TJSONObject = nil): string;
var
  ARequest: string;
  HttpClient: THTTPClient;
  ST: TStream;
  Response: IHTTPResponse;
begin
  // TODO: Validate URL and Authorization properties;
  if Variables = nil then
    ARequest := '{"query": "' + Query + '" }'
  else
    ARequest := '{"query": "' + Query + '",' + '"variables": ' +
      Variables.ToString + ' }';
  HttpClient := THTTPClient.Create;
  try
    HttpClient.ContentType := 'application/json';
    // Request.CharSet := 'utf-8'
    // HttpClient.Accept := CONTENTTYPE_APPLICATION_JSON;
    ST := TStringStream.Create(ARequest);
    try
      if Authorization = '' then
        Response := HttpClient.Post(URL, ST, nil, nil)
      else
        Response := HttpClient.Post(URL, ST, nil,
          TNetHeaders.Create(TNameValuePair.Create('Authorization',
          Authorization)));
      Result := Response.ContentAsString;
    finally
      ST.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

function TGraphQL.CallQuery(const Query: string;
  const Variables: String): string;
begin
  raise Exception.Create('Not implemented yet');
end;

procedure TGraphQL.SetAuthorization(const Value: string);
begin
  FAuthorization := Value;
end;

procedure TGraphQL.SetURL(const Value: string);
begin
  FURL := Value;
end;

end.
