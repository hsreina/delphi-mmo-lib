unit MMO.ClientList;

interface

uses
  Generics.Collections, MMO.ServerClient, MMO.Types;

type
  TClientList<ClientType> = class(TList<TServerClient<ClientType>>)
    private
      var m_sessionId: TSessionId;
      var m_lastInsertedIndex: Integer;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(const Value: TServerClient<ClientType>): TSessionId;
  end;

implementation

constructor TClientList<ClientType>.Create;
begin
  inherited;
  m_sessionId := 0;
end;

destructor TClientList<ClientType>.Destroy;
begin
  inherited;
end;

function TClientList<ClientType>.Add(const Value: TServerClient<ClientType>): TSessionId;
var
  realResult: integer;
begin
  m_lastInsertedIndex := inherited Add(Value);
  Result := m_sessionId;
  Inc(m_sessionId);
end;

end.
