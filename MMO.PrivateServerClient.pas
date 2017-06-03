unit MMO.PrivateServerClient;

interface

uses MMO.ServerClient, MMO.Types;

type
  TPrivateServerClient<ClientType> = class(TServerClient<ClientType>)
  public
    procedure SetSessionId(const sessionId: TSessionId);
  end;

implementation

procedure TPrivateServerClient<ClientType>.SetSessionId(const sessionId: TSessionId);
begin
  self.m_sessionId := sessionId;
end;

end.
