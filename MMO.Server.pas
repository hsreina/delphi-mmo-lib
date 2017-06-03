unit MMO.Server;

interface

uses
  IdTCPServer, IdSchedulerOfThreadPool, IdContext, SysUtils, IdGlobal, IdComponent,
  MMO.ServerClient, MMO.PacketReader, MMO.ClientList, SyncObjs, MMO.Lock,
  MMO.PrivateServerClient, MMO.Types, MMO.ServerCreateOptions;

type

  TTest = reference to procedure;

  TServer<ClientType> = class abstract
    private
      var m_clients: TClientList<ClientType>;
      var m_server: TIdTCPServer;
      var m_idSchedulerOfThreadPool: TIdSchedulerOfThreadPool;
      var m_maxPlayers: UInt16;

      var m_lock: TLock;

      procedure ServerOnConnect(AContext: TIdContext);
      procedure ServerOnDisconnect(AContext: TIdContext);
      procedure ServerOnExecute(AContext: TIdContext);
      procedure ServerOnException(AContext: TIdContext; AException: Exception);
      procedure ServerOnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);

      function GetClientByContext(AContext: TIdContext): TServerClient<ClientType>;

      procedure SetContextData(AContext: TIdContext; data: TObject);
      function GetContextData(AContext: TIdContext): TObject;
      procedure GuardAgainstInvalidOptions(const serverCreateOptions: TServerCreateOptions);
    protected
      procedure OnReceiveClientPacket(const client: TServerClient<ClientType>; const packetReader: TPacketReader); virtual;
      procedure OnClientCreate(const client: TServerClient<ClientType>); virtual;
      procedure OnClientDestroy(const client: TServerClient<ClientType>); virtual;
      procedure OnClientConnect(const client: TServerClient<ClientType>); virtual;
      procedure OnClientDisconnect(const client: TServerClient<ClientType>); virtual;
    public
      constructor Create(const serverCreateOptions: TServerCreateOptions);
      destructor Destroy; override;
      procedure Start;
  end;

implementation

uses
  IdTCPConnection;

constructor TServer<ClientType>.Create(const serverCreateOptions: TServerCreateOptions);
begin
  inherited Create;

  GuardAgainstInvalidOptions(serverCreateOptions);

  m_clients := TClientList<ClientType>.Create;

  m_maxPlayers := serverCreateOptions.MaxPlayers;

  m_lock := TLock.Create(serverCreateOptions.ThreadSafe);

  m_idSchedulerOfThreadPool := TIdSchedulerOfThreadPool.Create(nil);
  m_idSchedulerOfThreadPool.MaxThreads := m_maxPlayers + 1;
  m_idSchedulerOfThreadPool.PoolSize := m_maxPlayers + 1;

  m_server := TIdTCPServer.Create(nil);
  m_server.DefaultPort := serverCreateOptions.Port;

  m_server.MaxConnections := m_maxPlayers;
  m_server.ListenQueue := m_maxPlayers;

  m_server.OnExecute := ServerOnExecute;
  m_server.OnConnect := ServerOnConnect;
  m_server.OnDisconnect := ServerOnDisconnect;
  m_server.OnException := ServerOnException;
  m_server.OnStatus := ServerOnStatus;

  m_server.Scheduler := m_idSchedulerOfThreadPool;
end;

destructor TServer<ClientType>.Destroy;
begin
  m_server.Free;
  m_clients.Free;
  m_idSchedulerOfThreadPool.Free;
  m_lock.Free;
  inherited;
end;

procedure TServer<ClientType>.Start;
begin
  m_server.Active := true;
end;

procedure TServer<ClientType>.ServerOnConnect(AContext: TIdContext);
var
  client: TPrivateServerClient<ClientType>;
  sessionId: TSessionId;
begin
  m_lock.Synchronize(procedure
  var
    clientsCount: Integer;
  begin
    clientsCount := m_clients.Count;
    // WriteLn(String.Format('clients count : %d/%d', [clientsCount, m_maxPlayers]));
    if clientsCount >= m_maxPlayers then
    begin
      // WriteLn('refuse this client');
      AContext.Connection.DisconnectNotifyPeer;
      Exit;
    end;

    client := TPrivateServerClient<ClientType>.Create(AContext);

    sessionId := m_clients.Add(client);
    client.SetSessionId(sessionId);

    SetContextData(AContext, client);

    OnClientCreate(client);

    self.OnClientConnect(client);
  end);
end;

procedure TServer<ClientType>.ServerOnDisconnect(AContext: TIdContext);
var
  client: TServerClient<ClientType>;
begin
  m_lock.Synchronize(procedure
  begin
    client := GetClientByContext(AContext);
    if client = nil then
    begin
      Exit;
    end;

    SetContextData(AContext, nil);
    self.OnClientDisconnect(client);
    m_clients.Remove(client);
    self.OnClientDestroy(client);
    client.Free;
  end);
end;

procedure TServer<ClientType>.ServerOnExecute(AContext: TIdContext);
var
  buffer: TIdBytes;
  bufferSize: UInt32;
  packetReader: TPacketReader;
  client: TServerClient<ClientType>;
  idTCPConnection: TIdTCPConnection;
  packetHeader: TPacketHeader;
begin

  idTCPConnection := AContext.Connection;

  with idTCPConnection.IOHandler do
  begin
    idTCPConnection.IOHandler.ReadBytes(buffer, SizeOfTPacketHeader, False);
    move(buffer[0], packetHeader, SizeOfTPacketHeader);
    ReadBytes(buffer, packetHeader.Size, False);
  end;

  bufferSize := Length(buffer);

  if not (packetHeader.Size = bufferSize) then
  begin
    idTCPConnection.DisconnectNotifyPeer;
    Exit;
  end;

  client := GetClientByContext(AContext);
  if nil = client then
  begin
    idTCPConnection.DisconnectNotifyPeer;
    Exit;
  end;

  packetReader := TPacketReader.CreateFromBytesArray(buffer);
  m_lock.Synchronize(procedure
  begin
    self.OnReceiveClientPacket(client, packetReader);
  end);
  packetReader.Free;
end;

procedure TServer<ClientType>.ServerOnException(AContext: TIdContext; AException: Exception);
begin
  m_lock.Synchronize(procedure
  begin

  end);
end;

procedure TServer<ClientType>.ServerOnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  m_lock.Synchronize(procedure
  begin

  end);
end;

function TServer<ClientType>.GetClientByContext(AContext: TIdContext): TServerClient<ClientType>;
var
  Client: TServerClient<ClientType>;
  contextObject: TObject;
begin
  contextObject := GetContextData(AContext);

  for Client in m_clients do
  begin
    if client = contextObject then
    begin
      Exit(client);
    end;
  end;

  Exit(nil);
end;

procedure TServer<ClientType>.SetContextData(AContext: TIdContext; data: TObject);
begin
  {$IFDEF USE_OBJECT_ARC}
  AContext.DataObject := data;
  {$ELSE}
  AContext.Data := data;
  {$ENDIF}
end;

function TServer<ClientType>.GetContextData(AContext: TIdContext): TObject;
begin
  {$IFDEF USE_OBJECT_ARC}
  Exit(AContext.DataObject);
  {$ELSE}
  Exit(AContext.Data);
  {$ENDIF}
end;

procedure TServer<ClientType>.OnReceiveClientPacket(const client: TServerClient<ClientType>; const packetReader: TPacketReader);
begin

end;

procedure TServer<ClientType>.OnClientCreate(const client: TServerClient<ClientType>);
begin

end;

procedure TServer<ClientType>.OnClientDestroy(const client: TServerClient<ClientType>);
begin

end;

procedure TServer<ClientType>.OnClientConnect(const client: TServerClient<ClientType>);
begin

end;

procedure TServer<ClientType>.OnClientDisconnect(const client: TServerClient<ClientType>);
begin

end;

procedure TServer<ClientType>.GuardAgainstInvalidOptions(const serverCreateOptions: TServerCreateOptions);
const
  cMaxPlayers = 500;
var
  maxPlayers: UInt16;
begin
  maxPlayers := serverCreateOptions.MaxPlayers;

  if maxPlayers = 0 then
  begin
    raise Exception.Create('You must specify max players');
  end;

  if maxPlayers > cMaxPlayers then
  begin
    raise Exception.Create(String.Format('You can''t assign more than %d players', [cMaxPlayers]));
  end;
end;

end.

