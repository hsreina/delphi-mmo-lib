unit MMO.Packet;

interface

uses
  Classes;

type
  TPacket = class abstract
    private
    protected
      var m_data: TMemoryStream;
      function GetMemory: Pointer;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Skip(count: integer);
      function Seek(offset, origin: integer): integer;
      function GetSize: UInt32;

      function CreateStream: TStream;
      property Memory: Pointer read GetMemory;

      procedure Log;
  end;

implementation

constructor TPacket.Create;
begin
  inherited;
  m_data := TMemoryStream.Create;
end;

destructor TPacket.Destroy;
begin
  m_data.Free;
  inherited;
end;

procedure TPacket.Skip(count: integer);
begin
  Seek(count, 1);
end;

function TPacket.Seek(offset, origin: integer): integer;
begin
  Exit(m_data.Seek(offset, origin));
end;

function TPacket.GetSize;
begin
  Result := m_data.Size;
end;

function TPacket.CreateStream;
var
  stream: TMemoryStream;
  savedPos: Int64;
begin
  stream := TMemoryStream.Create;

  savedPos := m_data.Position;
  m_data.Position := 0;
  stream.CopyFrom(m_data, m_data.Size);
  m_data.Position := savedPos;
  stream.Position := 0;

  Result := stream;
end;

function TPacket.GetMemory;
begin
  Result := m_data.Memory;
end;

procedure TPacket.Log;
begin

end;

end.
