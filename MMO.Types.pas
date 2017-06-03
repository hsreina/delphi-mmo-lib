unit MMO.Types;

interface

type
  TSessionId = UInt32;

  TPacketHeader = packed record
    Size: UInt32;
  end;

const
  SizeOfTPacketHeader = SizeOf(TPacketHeader);

implementation

end.
