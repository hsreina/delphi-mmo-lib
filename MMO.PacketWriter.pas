unit MMO.PacketWriter;

interface

uses MMO.Packet;

type
  TPacketWriter = class(TPacket)
    private
    public
      function WriteUInt8(const src: UInt8): Boolean;
      function WriteUInt16(const src: UInt16): Boolean;
      function WriteUInt32(const src: UInt32): Boolean;
      function WriteInt32(const src: Int32): Boolean;
      function WriteUInt64(const src: UInt64): Boolean;
      function WriteInt64(const src: Int64): Boolean;
      function Write(const src; const count: UInt32): Boolean;
      function WriteDouble(const src: Double): boolean;
      function WriteStr(const src: AnsiString): Boolean; overload;
      function WriteStr(const src: AnsiString; const count: UInt32): Boolean; overload;
      function WriteStr(const src: AnsiString; const count: UInt32; const overflow: AnsiChar): Boolean; overload;
  end;

implementation

function TPacketWriter.WriteUInt8(const src: UInt8): Boolean;
begin
  Exit(Write(src, 1));
end;

function TPacketWriter.WriteUInt16(const src: UInt16): Boolean;
begin
  Exit(Write(src, 2));
end;

function TPacketWriter.WriteUInt32(const src: UInt32): Boolean;
begin
  Exit(Write(src, 4));
end;

function TPacketWriter.WriteInt32(const src: Int32): Boolean;
begin
  Exit(Write(src, 4));
end;

function TPacketWriter.WriteUInt64(const src: UInt64): Boolean;
begin
  Exit(Write(src, 8));
end;

function TPacketWriter.WriteInt64(const src: Int64): Boolean;
begin
  Exit(Write(src, 8));
end;

function TPacketWriter.Write(const src; const count: Cardinal): Boolean;
begin
  Result := m_data.Write(src, count) = count;
end;

function TPacketWriter.WriteDouble(const src: Double): boolean;
begin
  Exit(Write(src, 4));
end;

function TPacketWriter.WriteStr(const src: AnsiString; const count: UInt32): Boolean;
begin
  Exit(WriteStr(src, count, #$00));
end;

function TPacketWriter.WriteStr(const src: AnsiString; const count: UInt32; const overflow: AnsiChar): Boolean;
var
  dataSize: UInt32;
  remainingDataSize: integer;
  remainningData: AnsiString;
begin
  dataSize := Length(src);
  if dataSize < count then
  begin
    Result := Write(src[1], dataSize);
    remainingDataSize :=  count - dataSize;
    remainningData := StringOfChar(overflow, remainingDataSize);
    Result := Result and Write(remainningData[1], remainingDataSize);
  end else begin
    Exit(Write(src[1], count));
  end;
end;

function TPacketWriter.WriteStr(const src: AnsiString): Boolean;
var
  size: UInt16;
begin
  size := Length(src);
  WriteUInt16(size);
  Write(src[1], size);
end;

end.
