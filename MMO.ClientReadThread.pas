unit MMO.ClientReadThread;

interface

uses
  Classes, IdTCPClient, MMO.PacketReader, IdIOHandler, IdGlobal, MMO.Types;

type

  TClientReadThread = class;

  TSyncClientReadThreadReadEvent = procedure(const sender: TObject; const packetReader: TPacketReader) of object;

  TClientReadThread = class(TThread)
    private
      var m_client: TIdTCPClient;
      var m_onRead: TSyncClientReadThreadReadEvent;
      var m_name: string;
      procedure Execute; override;
      procedure TriggerReadEvent(const packetReader: TPacketReader);
      function TryReadBuffer(const ioHandler: TIdIOHandler; var buffer: TIdBytes; var packetHeader: TPacketHeader): Boolean;
    public
      constructor Create(const threadName: string; const client: TIdTCPClient);
      destructor Destroy; override;
      property OnRead: TSyncClientReadThreadReadEvent read m_onRead write m_onRead;
  end;

implementation

constructor TClientReadThread.Create(const threadName: string; const client: TIdTCPClient);
begin
  inherited Create(True);
  m_name := threadName;
  m_client := client;
end;

destructor TClientReadThread.Destroy;
begin
  inherited;
end;

procedure TClientReadThread.Execute;
var
  ioHandler: TIdIOHandler;
  buffer: TIdBytes;
  bufferSize, dataSize: UInt32;
  packetReader: TPacketReader;
  packetHeader: TPacketHeader;
begin
  inherited;
  NameThreadForDebugging(m_name + 'ReadThread', self.ThreadID);

  while not Terminated do
  begin

    if not m_client.Connected then
    begin
      Sleep(100);
      continue;
    end;

    ioHandler := m_client.IOHandler;

    SetLength(buffer, 0);

    if not TryReadBuffer(ioHandler, buffer, packetHeader) then
    begin
      break;
    end;

    dataSize := packetHeader.Size;

    bufferSize := Length(buffer);
    if not (dataSize = bufferSize) then
    begin
      m_client.Disconnect;
      Exit;
    end;

    packetReader := TPacketReader.CreateFromBytesArray(buffer);
    self.TriggerReadEvent(packetReader);
    packetReader.Free;
  end;
end;

function TClientReadThread.TryReadBuffer(const ioHandler: TIdIOHandler; var buffer: TIdBytes; var packetHeader: TPacketHeader): Boolean;
begin
  try
    ioHandler.ReadBytes(buffer, SizeOfTPacketHeader, False);
    move(buffer[0], packetHeader, SizeOfTPacketHeader);
    ioHandler.ReadBytes(buffer, packetHeader.Size, False);
  except
    Exit(False);
  end;

  Exit(True);
end;

procedure TClientReadThread.TriggerReadEvent(const packetReader: TPacketReader);
begin
  if Assigned(m_onRead) then
  begin
    m_onRead(self, packetReader);
  end;
end;

end.
