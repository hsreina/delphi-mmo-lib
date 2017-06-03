unit MMO.PacketReader;

interface

uses MMO.Packet, SysUtils;

type
  TPacketReader = class(TPacket)
    private
      function Write(const src; const count: UInt32): Boolean;
    public
      constructor CreateFromBytesArray(const src: array of Byte);
      function ReadUInt8(var dst: UInt8): Boolean;
      function ReadUInt16(var dst: UInt16): Boolean;
      function ReadUInt32(var dst: UInt32): Boolean;
      function ReadInt32(var dst: Int32): Boolean;
      function ReadUInt64(var dst: UInt64): Boolean;
      function ReadInt64(var dst: Int64): Boolean;
      function Read(var dst; const count: UInt32): Boolean;
      function ReadDouble(var dst: Double): boolean;
      function ReadStr(var dst: AnsiString; count: UInt32): Boolean; overload;
      function ReadStr(var dst: AnsiString): Boolean; overload;
  end;

implementation

constructor TPacketReader.CreateFromBytesArray(const src: array of Byte);
begin
  inherited Create;
  Write(src[0], Length(src));
  Seek(0, 0);
end;

function TPacketReader.ReadUInt8(var dst: UInt8): Boolean;
begin
  Exit(Read(dst, 1));
end;

function TPacketReader.ReadUInt16(var dst: UInt16): Boolean;
begin
  Exit(Read(dst, 2));
end;

function TPacketReader.ReadUInt32(var dst: UInt32): Boolean;
begin
  Exit(Read(dst, 4));
end;

function TPacketReader.ReadInt32(var dst: Int32): Boolean;
begin
  Exit(Read(dst, 4));
end;

function TPacketReader.ReadUInt64(var dst: UInt64): Boolean;
begin
  Exit(Read(dst, 8));
end;

function TPacketReader.ReadInt64(var dst: Int64): Boolean;
begin
  Exit(Read(dst, 8));
end;

function TPacketReader.Read(var dst; const count: Cardinal): Boolean;
begin
  Result := m_data.Read(dst, count) = count;
end;

function TPacketReader.ReadDouble(var dst: Double): boolean;
begin
  Exit(Read(dst, 4));
end;

function TPacketReader.ReadStr(var dst: AnsiString; count: UInt32): Boolean;
begin
  SetLength(dst, count);
  Exit(Read(dst[1], count));
end;

function TPacketReader.ReadStr(var dst: AnsiString): Boolean;
var
  size: UInt16;
begin
  if not ReadUint16(size) then
  begin
    Exit(False);
  end;

  setLength(dst, size);
  Exit(Read(dst[1], size));
end;

function TPacketReader.Write(const src; const count: Cardinal): Boolean;
begin
  Result := m_data.Write(src, count) = count;
end;

end.
