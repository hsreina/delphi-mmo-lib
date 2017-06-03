unit MMO.ServerClient;

interface

uses
  IdContext, MMO.Packet, Classes, MMO.Types;

type
  TServerClient<ClientType> = class abstract
    private
      var m_context: TIdContext;
    protected
      var m_sessionId: TSessionId;
    public
      constructor Create(AContext: TIdContext);
      destructor Destroy; override;
      property SessionId: TSessionId read m_sessionId;
      procedure Send(const packet: TPacket);
      var Data: ClientType;
  end;

implementation

constructor TServerClient<ClientType>.Create(AContext: TIdContext);
begin
  inherited Create;
  m_context := AContext;
end;

destructor TServerClient<ClientType>.Destroy;
begin
  inherited;
end;

procedure TServerClient<ClientType>.Send(const packet: TPacket);
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

  packetHeader.Size := dataSize;

  memory := packet.Memory;

  memoryStream := TMemoryStream.Create;
  memoryStream.Write(packetHeader, SizeOfTPacketHeader);
  memoryStream.Write(memory^, dataSize);

  m_context.Connection.IOHandler.Write(memoryStream);

  memoryStream.Free;
end;

end.
