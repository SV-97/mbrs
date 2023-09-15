use std::io::Read;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum AddrScheme {
    Chs,
    Lba,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum PartType {
    Empty,
    Fat12 {
        visible: bool,
    },
    Oem,
    Fat16 {
        visible: bool,
        leq32mib: bool,
        scheme: AddrScheme,
    },
    Extended {
        scheme: AddrScheme,
    },
    /// Might also be HPFS or NTFS
    ExFAT {
        visible: bool,
    },
    Fat32 {
        visible: bool,
        scheme: AddrScheme,
    },
    WindowsRe,
    DynamicDisk,
    Gpfs,
    LinuxSwap,
    LinuxNative,
    IntelRapidStart,
    LinuxLvm,
    FreeBsd,
    OpenBsd,
    NetBsd,
    MacOs,
    Solaris,
    BeOs,
    ProtectiveMbr,
    Efi,
    LinuxRaid,
}

impl TryFrom<u8> for PartType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use AddrScheme::*;
        use PartType::*;

        match value {
            0x00 => Ok(Empty),
            0x01 => Ok(Fat12 { visible: true }),
            0x02 => Ok(Fat12 { visible: false }),
            0x12 => Ok(Oem),
            0x04 => Ok(Fat16 {
                visible: true,
                leq32mib: true,
                scheme: Chs,
            }),
            0x14 => Ok(Fat16 {
                visible: false,
                leq32mib: true,
                scheme: Chs,
            }),
            0x05 => Ok(Extended { scheme: Chs }),
            0x06 => Ok(Fat16 {
                visible: true,
                leq32mib: false,
                scheme: Chs,
            }),
            0x07 => Ok(ExFAT { visible: true }),
            0x17 => Ok(ExFAT { visible: false }),
            0x0B => Ok(Fat32 {
                visible: true,
                scheme: Chs,
            }),
            0x1B => Ok(Fat32 {
                visible: false,
                scheme: Chs,
            }),
            0x0C => Ok(Fat32 {
                visible: true,
                scheme: Lba,
            }),
            0x1C => Ok(Fat32 {
                visible: false,
                scheme: Lba,
            }),
            0x0E => Ok(Fat16 {
                visible: true,
                leq32mib: false,
                scheme: Lba,
            }),
            0x1E => Ok(Fat16 {
                visible: false,
                leq32mib: false,
                scheme: Lba,
            }),
            0x0F => Ok(Extended { scheme: Lba }),
            0x27 => Ok(WindowsRe),
            0x42 => Ok(DynamicDisk),
            0x75 => Ok(Gpfs),
            0x82 => Ok(LinuxSwap),
            0x83 => Ok(LinuxNative),
            0x84 => Ok(IntelRapidStart),
            0x8E => Ok(LinuxLvm),
            0xA5 => Ok(FreeBsd),
            0xA6 => Ok(OpenBsd),
            0xA9 => Ok(NetBsd),
            0xAF => Ok(MacOs),
            0xBF => Ok(Solaris),
            0xEB => Ok(BeOs),
            0xEE => Ok(ProtectiveMbr),
            0xEF => Ok(Efi),
            0xFD => Ok(LinuxRaid),
            _ => Err(()),
        }
    }
}

impl TryFrom<PartType> for u8 {
    type Error = ();
    fn try_from(part_type: PartType) -> Result<Self, Self::Error> {
        use AddrScheme::*;
        use PartType::*;

        match part_type {
            Empty => Ok(0x00),
            Fat12 { visible: true } => Ok(0x01),
            Fat12 { visible: false } => Ok(0x02),
            Oem => Ok(0x12),
            Fat16 {
                visible: true,
                leq32mib: true,
                scheme: Chs,
            } => Ok(0x04),
            Fat16 {
                visible: false,
                leq32mib: true,
                scheme: Chs,
            } => Ok(0x14),
            Extended { scheme: Chs } => Ok(0x05),
            Fat16 {
                visible: true,
                leq32mib: false,
                scheme: Chs,
            } => Ok(0x06),
            ExFAT { visible: true } => Ok(0x07),
            ExFAT { visible: false } => Ok(0x17),
            Fat32 {
                visible: true,
                scheme: Chs,
            } => Ok(0x0B),
            Fat32 {
                visible: false,
                scheme: Chs,
            } => Ok(0x1B),
            Fat32 {
                visible: true,
                scheme: Lba,
            } => Ok(0x0C),
            Fat32 {
                visible: false,
                scheme: Lba,
            } => Ok(0x1C),
            Fat16 {
                visible: true,
                leq32mib: false,
                scheme: Lba,
            } => Ok(0x0E),
            Fat16 {
                visible: false,
                leq32mib: false,
                scheme: Lba,
            } => Ok(0x1E),
            Extended { scheme: Lba } => Ok(0x0F),
            WindowsRe => Ok(0x27),
            DynamicDisk => Ok(0x42),
            Gpfs => Ok(0x75),
            LinuxSwap => Ok(0x82),
            LinuxNative => Ok(0x83),
            IntelRapidStart => Ok(0x84),
            LinuxLvm => Ok(0x8E),
            FreeBsd => Ok(0xA5),
            OpenBsd => Ok(0xA6),
            NetBsd => Ok(0xA9),
            MacOs => Ok(0xAF),
            Solaris => Ok(0xBF),
            BeOs => Ok(0xEB),
            ProtectiveMbr => Ok(0xEE),
            Efi => Ok(0xEF),
            LinuxRaid => Ok(0xFD),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ChsEntry {
    pub raw: [u8; 3],
}

impl ChsEntry {
    /// Note that only the 6 bottom bits of sector and 10 bottom bits of cylinder will be used
    pub fn new(head: u8, sector: u8, cylinder: u16) -> Self {
        ChsEntry {
            raw: [
                head,
                sector & 0x1f | ((cylinder & 0x300) >> 2) as u8,
                (cylinder & 0xFF) as u8,
            ],
        }
    }

    pub fn head(&self) -> u8 {
        self.raw[0]
    }

    pub fn sector(&self) -> u8 {
        self.raw[1] & 0xF1
    }

    pub fn cylinder(&self) -> u16 {
        self.raw[2] as u16 | (((self.raw[1] & 0xc0) as u16) << 2)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct PartInfo {
    bootable: bool,
    first_sector_chs: ChsEntry,
    part_type: PartType,
    last_sector_chs: ChsEntry,
    start_sector_lba: u32,
    sector_count_lba: u32,
}

impl PartInfo {
    pub fn is_zeroed(&self) -> bool {
        !self.bootable
            && self.first_sector_chs.raw == [0, 0, 0]
            && self.part_type == PartType::Empty
            && self.last_sector_chs.raw == [0, 0, 0]
            && self.start_sector_lba == 0
            && self.sector_count_lba == 0
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum PartInfoErr {
    UnclearBootable,
    UnknownPartType,
}

impl TryFrom<[u8; 16]> for PartInfo {
    type Error = PartInfoErr;
    fn try_from(buf: [u8; 16]) -> Result<Self, Self::Error> {
        let bootable = match buf[0] {
            0x80 => Ok(true),
            0x00 => Ok(false),
            _ => Err(PartInfoErr::UnclearBootable),
        }?;
        let part_type = PartType::try_from(buf[4]).map_err(|_| PartInfoErr::UnknownPartType)?;

        Ok(PartInfo {
            bootable,
            part_type,
            first_sector_chs: ChsEntry {
                raw: buf[1..4].try_into().unwrap(),
            },
            last_sector_chs: ChsEntry {
                raw: buf[5..8].try_into().unwrap(),
            },
            start_sector_lba: u32::from_le_bytes(buf[8..12].try_into().unwrap()),
            sector_count_lba: u32::from_le_bytes(buf[12..16].try_into().unwrap()),
        })
    }
}

impl TryFrom<PartInfo> for [u8; 16] {
    type Error = PartInfoErr;
    fn try_from(part: PartInfo) -> Result<Self, Self::Error> {
        let mut buf = [0; 16];
        buf[0] = if part.bootable { 0x80 } else { 0x00 };
        buf[4] = u8::try_from(part.part_type).map_err(|_| PartInfoErr::UnknownPartType)?;
        buf[1..4].copy_from_slice(&part.first_sector_chs.raw);
        buf[5..8].copy_from_slice(&part.last_sector_chs.raw);
        buf[8..12].copy_from_slice(&part.start_sector_lba.to_le_bytes());
        buf[12..16].copy_from_slice(&part.sector_count_lba.to_le_bytes());
        Ok(buf)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MbrPartTable {
    pub entries: [Option<PartInfo>; 4],
}

impl From<[u8; 64]> for MbrPartTable {
    fn from(buf: [u8; 64]) -> Self {
        let from_slice = |buf| match PartInfo::try_from(<[u8; 16]>::try_from(buf).unwrap()).ok() {
            // We'll convert empty partition types into Nones here
            Some(p) if p.is_zeroed() => None,
            x => x,
        };
        MbrPartTable {
            entries: [
                from_slice(&buf[0..16]),
                from_slice(&buf[16..32]),
                from_slice(&buf[32..48]),
                from_slice(&buf[48..64]),
            ],
        }
    }
}

impl TryFrom<MbrPartTable> for [u8; 64] {
    type Error = PartInfoErr;
    fn try_from(tbl: MbrPartTable) -> Result<Self, Self::Error> {
        let mut buf = [0; 64];
        buf[0..16].copy_from_slice(&match tbl.entries[0] {
            Some(e) => <[u8; 16]>::try_from(e)?,
            _ => Default::default(),
        });
        buf[16..32].copy_from_slice(&match tbl.entries[1] {
            Some(e) => <[u8; 16]>::try_from(e)?,
            _ => Default::default(),
        });
        buf[32..48].copy_from_slice(&match tbl.entries[2] {
            Some(e) => <[u8; 16]>::try_from(e)?,
            _ => Default::default(),
        });
        buf[48..64].copy_from_slice(&match tbl.entries[3] {
            Some(e) => <[u8; 16]>::try_from(e)?,
            _ => Default::default(),
        });
        Ok(buf)
    }
}

impl MbrPartTable {
    pub fn try_from_reader<B>(mut reader: B) -> Result<Self, std::io::Error>
    where
        B: Read,
    {
        use std::io::{Error, ErrorKind};

        let mut buf = [0; 64];
        match reader.read(&mut buf) {
            Ok(64) => Ok(Self::from(buf)),
            Ok(n_bytes_read) => Err(Error::new(
                ErrorKind::UnexpectedEof,
                format!("Reader could not read 64 bytes for MBR partition table. Could only read {} bytes.", n_bytes_read),
            )),
            Err(e) => Err(e)
        }
    }
}
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Mbr {
    pub bootloader: [u8; 440],
    pub drive_signature: [u8; 4],
    pub partition_table: MbrPartTable,
    pub bootsector_signature: [u8; 2],
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum MbrProblem {
    NullSectorIsNotNull,
    BootloaderSignatureNotSet,
}

impl Mbr {
    pub fn try_from_reader<B>(mut reader: B) -> Result<Self, std::io::Error>
    where
        B: Read,
    {
        use std::io::{Error, ErrorKind};

        let mut bootloader = [0; 440];
        match reader.read(&mut bootloader) {
            Ok(440) => Ok(()),
            Ok(n_bytes_read) => Err(Error::new(
                ErrorKind::UnexpectedEof,
                format!(
                    "Could not read 440 bytes for MBR bootloader. Could only read {} bytes.",
                    n_bytes_read
                ),
            )),
            Err(e) => Err(e),
        }?;

        let mut drive_signature = [0; 4];
        match reader.read(&mut drive_signature) {
            Ok(4) => Ok(()),
            Ok(n_bytes_read) => Err(Error::new(
                ErrorKind::UnexpectedEof,
                format!(
                    "Could not read 4 bytes for MBR drive signature. Could only read {} bytes.",
                    n_bytes_read
                ),
            )),
            Err(e) => Err(e),
        }?;

        let mut zero_buf = [0; 2];
        match reader.read(&mut zero_buf) {
            Ok(2) if zero_buf == [0, 0] => Ok(()),
            // Copy protected according to spec on wikipedia - we'll just ignore it
            Ok(2) if zero_buf == [0x5A, 0x5A] => Ok(()),
            Ok(2) => Err(Error::new(
                ErrorKind::InvalidData,
                format!(
                    "MBR null sector is not null but rather {:02x} {:02x}",
                    zero_buf[0], zero_buf[1]
                ),
            )),
            Ok(n_bytes_read) => Err(Error::new(
                ErrorKind::UnexpectedEof,
                format!(
                    "Could not read 2 bytes for MBR null sector. Could only read {} bytes.",
                    n_bytes_read
                ),
            )),
            Err(e) => Err(e),
        }?;

        let partition_table = MbrPartTable::try_from_reader(&mut reader)?;
        let mut bootsector_signature = [0; 2];

        match reader.read(&mut bootsector_signature) {
            Ok(2) => Ok(()),
            Ok(n_bytes_read) => Err(Error::new(
                ErrorKind::UnexpectedEof,
                format!("Could not read the required 2 bytes for MBR bootsector signature. Could only read {} bytes.", n_bytes_read),
            )),
            Err(e) => Err(e),
        }?;
        Ok(Self {
            bootloader,
            drive_signature,
            partition_table,
            bootsector_signature,
        })
    }
}

impl TryFrom<&Mbr> for [u8; 512] {
    type Error = PartInfoErr;
    fn try_from(mbr: &Mbr) -> Result<Self, Self::Error> {
        let mut buf = [0; 512];
        buf[0..440].copy_from_slice(&mbr.bootloader);
        buf[440..444].copy_from_slice(&mbr.drive_signature);
        // if we ever care about the copy protection it should go here
        // buf[444..446].copy_from_slice(&mbr.drive_signature);
        buf[446..510].copy_from_slice(&<[u8; 64]>::try_from(mbr.partition_table)?);
        buf[510..512].copy_from_slice(&mbr.bootsector_signature);
        Ok(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;

    #[test]
    fn read_mbr() {
        let raspios_img = File::open("./raspios.img").unwrap();
        let mbr = Mbr::try_from_reader(raspios_img).unwrap();
        dbg!(mbr);
        panic!()
    }
}
