unit MMO.Client;

interface

uses
  IdTcpClient, MMO.Packet, Classes, SysUtils, MMO.ClientReadThread,
  MMO.PacketReader, MMO.Types, IdComponent;

type
  TClient = class abstract
    private
      var m_client: TIdTcpClient;
      var m_clientReadThread: TClientReadThread;

      procedure OnClientConnected(sender: TObject);
      procedure OnClientDisconnected(sender: TObject);
      procedure OnClientRead(const sender: TObject; const packetReader: TPacketReader);
      procedure OnClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);

    protected
      constructor Create(host: string; port: UInt16);
      destructor Destroy; override;
      procedure Connect;
      procedure Disconnect;
      procedure Send(packet: TPacket);

      procedure OnConnected; virtual;
      procedure OnDisconnected; virtual;
      procedure OnReceivePacket(const packetReader: TPacketReader); virtual;
      procedure Sleep(ms: Integer);
  end;

implementation

uses
  Rtti;

constructor TClient.Create(host: string; port: UInt16);
begin
  inherited Create;
  m_client := TIdTcpClient.Create(nil);
  m_client.Port := port;
  m_client.Host := host;

  // m_client.OnBeforeBind
  // m_client.OnAfterBind
  // m_client.OnSocketAllocated
  m_client.OnConnected := OnClientConnected;
  m_client.OnDisconnected := OnClientDisconnected;
  // m_client.OnWork
  // m_client.OnWorkBegin
  // m_client.OnWorkEnd
  m_client.OnStatus := OnClientStatus;

  m_clientReadThread := TClientReadThread.Create('Client', m_client);
  m_clientReadThread.OnRead := OnClientRead;
end;

destructor TClient.Destroy;
begin
  m_client.Free;
  m_clientReadThread.Free;
  inherited;
end;

procedure TClient.Connect;
begin
  m_client.Connect;
  m_clientReadThread.Start;
end;

procedure TClient.Disconnect;
begin
  m_clientReadThread.Terminate;
  m_client.Disconnect;
end;

procedure TClient.Send(packet: TPacket);
var
  memoryStream: TMemoryStream;
  dataSize: UInt32;
  memory: Pointer;
  packetHeader: TPacketHeader;
begin
  dataSize := packet.GetSize;

  if dataSize = 0 then
  begin
    Exit;
  end;

  memory := packet.Memory;

  packetHeader.Size := dataSize;

  memoryStream := TMemoryStream.Create;
  memoryStream.Write(packetHeader, SizeOfTpacketHeader);
  memoryStream.Write(memory^, dataSize);

  m_client.IOHandler.Write(memoryStream);

  memoryStream.Free;
end;

procedure TClient.OnClientConnected(sender: TObject);
begin
  OnConnected;
end;

procedure TClient.OnClientDisconnected(sender: TObject);
begin
  OnDisconnected;
end;

procedure TClient.OnClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  //WriteLn(String.Format('TClient.OnClientStatus %s', [TRttiEnumerationType.GetName(AStatus)]));
  case AStatus of
    hsResolving: ;
    hsConnecting: ;
    hsConnected: ;
    hsDisconnecting: ;
    hsDisconnected:
    begin
      OnDisconnected;
    end;
    hsStatusText: ;
    ftpTransfer: ;
    ftpReady: ;
    ftpAborted: ;
  end;

end;

procedure TClient.OnClientRead(const sender: TObject; const packetReader: TPacketReader);
begin
  OnReceivePacket(packetReader);
end;

procedure TClient.OnConnected;
begin

end;

procedure TClient.OnDisconnected;
begin

end;

procedure TClient.OnReceivePacket(const packetReader: TPacketReader);
begin

end;

procedure TClient.Sleep(ms: Integer);
begin
  m_clientReadThread.Sleep(ms);
end;

end.
