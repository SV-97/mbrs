use std::io::Read;
use thiserror::Error;

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
    /// EXT3, EXT4, BTRFS etc.
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

impl PartType {
    /// returns the addressing scheme (CHS or LBA) used by partitions of the given type
    pub fn addr_scheme(&self) -> AddrScheme {
        match self {
            PartType::Fat32 { scheme, .. }
            | PartType::Fat16 { scheme, .. }
            | PartType::Extended { scheme, .. } => *scheme,
            PartType::LinuxNative => AddrScheme::Lba,
            _ => unimplemented!(),
        }
    }

    pub fn btrfs() -> Self {
        PartType::LinuxNative
    }

    pub fn ext4() -> Self {
        PartType::LinuxNative
    }
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

/// masks off the lowest 6 bits
const SECTOR_MASK: u8 = 0x3F;

impl ChsEntry {
    /// Note that only the 6 bottom bits of sector and 10 bottom bits of cylinder can be used for CHS.
    /// If a value leaves this range this method returns None
    // TODO: use https://crates.io/crates/arbitrary-int to fix the input types
    pub fn try_from_chs(cylinder: u16, head: u8, sector: u8) -> Option<Self> {
        if cylinder > 0x3FF || sector > 0x3F {
            None
        } else {
            Some(ChsEntry {
                raw: [
                    head,
                    (sector & SECTOR_MASK) | (((cylinder & 0x300) >> 2) as u8),
                    (cylinder & 0xFF) as u8,
                ],
            })
        }
    }

    pub fn cylinder(&self) -> u16 {
        (((self.raw[1] & !SECTOR_MASK) as u16) << 2) | (self.raw[2] as u16)
    }

    pub fn head(&self) -> u8 {
        self.raw[0]
    }

    pub fn sector(&self) -> u8 {
        self.raw[1] & SECTOR_MASK
    }

    pub fn chs(&self) -> (u16, u8, u8) {
        (self.cylinder(), self.head(), self.sector())
    }

    pub fn try_from_lba(lba: u32) -> Result<Self, MbrError> {
        // let heads_per_cylinder: u32 = 255;
        // let sectors_per_track: u32 = 63;
        let heads_per_cylinder: u32 = 4;
        let sectors_per_track: u32 = 32;

        let cylinder = lba / (heads_per_cylinder * sectors_per_track);
        let head = (lba / sectors_per_track) % heads_per_cylinder;
        let sector = lba % sectors_per_track + 1;
        ChsEntry::try_from_chs(
            u16::try_from(cylinder).unwrap(),
            u8::try_from(head).unwrap(),
            u8::try_from(sector).unwrap(),
        )
        .ok_or(MbrError::InvalidAddressChs {
            cylinder,
            head,
            sector,
        })
    }

    /// Like `try_from_lba` but silently truncates the input
    pub fn from_lba_truncating(lba: u32) -> Self {
        ChsEntry::try_from_lba(lba).unwrap_or_else(|e| {
            if let MbrError::InvalidAddressChs {
                cylinder,
                head,
                sector,
            } = e
            {
                // If we're using LBA we don't really care about the CHS entries being correct. We simply
                // truncate the "correct" values that CHS can't represent and use those instead.
                ChsEntry::try_from_chs(
                    cylinder as u16 & 0x3FF,
                    head as u8,
                    sector as u8 & SECTOR_MASK,
                )
                .unwrap()
            } else {
                panic!("Unexpected error: {}", e)
            }
        })
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

// partition!(10 B, *, 100 MiB, true, Linux { lba: true })

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct IncompletePartInfo {
    pub start_lba: Option<u32>,
    pub end_lba: Option<u32>,
    pub size_lba: Option<u32>,
    pub bootable: bool,
    pub part_type: PartType,
}

impl TryFrom<IncompletePartInfo> for PartInfo {
    type Error = MbrError;
    fn try_from(value: IncompletePartInfo) -> Result<Self, Self::Error> {
        // Use 0 = start + size - end - 1 in different forms to calculate missing fields
        let (start, size) = match (value.start_lba, value.end_lba, value.size_lba) {
            (Some(start), Some(_end), Some(size)) => Ok((start, size)),
            (Some(start), Some(end), None) => Ok((start, 1 + end - start)),
            (Some(start), None, Some(size)) => Ok((start, size)),
            (None, Some(end), Some(size)) => Ok((1 + end - size, end)),
            _ => Err(MbrError::IncompleteInput),
        }?;
        PartInfo::try_from_lba(value.bootable, start, size, value.part_type)
    }
}

impl PartInfo {
    pub fn try_from_lba(
        bootable: bool,
        start_sector_lba: u32,
        sector_count_lba: u32,
        part_type: PartType,
    ) -> Result<Self, MbrError> {
        let end_sector_lba = start_sector_lba + sector_count_lba - 1;
        match part_type.addr_scheme() {
            AddrScheme::Lba => Ok(Self {
                bootable,
                first_sector_chs: ChsEntry::from_lba_truncating(start_sector_lba),
                last_sector_chs: ChsEntry::from_lba_truncating(end_sector_lba),
                start_sector_lba,
                sector_count_lba,
                part_type,
            }),
            AddrScheme::Chs => {
                let first_sector_chs = ChsEntry::try_from_lba(start_sector_lba)?;
                let last_sector_chs = ChsEntry::try_from_lba(end_sector_lba)?;
                Ok(Self {
                    bootable,
                    first_sector_chs,
                    last_sector_chs,
                    start_sector_lba,
                    sector_count_lba,
                    part_type,
                })
            }
        }
    }

    pub fn try_from_lba_bounds(
        bootable: bool,
        start_sector_lba: u32,
        end_sector_lba: u32,
        part_type: PartType,
    ) -> Result<Self, MbrError> {
        let sector_count_lba = 1 + end_sector_lba - start_sector_lba;
        match part_type.addr_scheme() {
            AddrScheme::Lba => Ok(Self {
                bootable,
                first_sector_chs: ChsEntry::from_lba_truncating(start_sector_lba),
                last_sector_chs: ChsEntry::from_lba_truncating(end_sector_lba),
                start_sector_lba,
                sector_count_lba,
                part_type,
            }),
            AddrScheme::Chs => {
                let first_sector_chs = ChsEntry::try_from_lba(start_sector_lba)?;
                let last_sector_chs = ChsEntry::try_from_lba(end_sector_lba)?;
                Ok(Self {
                    bootable,
                    first_sector_chs,
                    last_sector_chs,
                    start_sector_lba,
                    sector_count_lba,
                    part_type,
                })
            }
        }
    }

    pub fn is_zeroed(&self) -> bool {
        !self.bootable
            && self.first_sector_chs.raw == [0, 0, 0]
            && self.part_type == PartType::Empty
            && self.last_sector_chs.raw == [0, 0, 0]
            && self.start_sector_lba == 0
            && self.sector_count_lba == 0
    }

    pub fn end_sector_lba(&self) -> u32 {
        self.start_sector_lba + self.sector_count_lba - 1
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
    pub fn try_from_reader<B>(mut reader: B) -> Result<Self, MbrError>
    where
        B: Read,
    {
        let mut buf = [0; 64];
        match reader.read(&mut buf) {
            Ok(64) => Ok(Self::from(buf)),
            Ok(n_bytes_read) => Err(MbrError::InputTooSmall {
                input_size: n_bytes_read,
                error_location: "partition table",
            }),
            Err(e) => Err(MbrError::IoError(e.to_string())),
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

impl Mbr {
    pub fn try_from_reader<B>(mut reader: B) -> Result<Self, MbrError>
    where
        B: Read,
    {
        let mut bootloader = [0; 440];
        match reader.read(&mut bootloader) {
            Ok(440) => Ok(()),
            Ok(n_bytes_read) => Err(MbrError::InputTooSmall {
                input_size: n_bytes_read,
                error_location: "bootloader",
            }),
            Err(e) => Err(MbrError::IoError(e.to_string())),
        }?;

        let mut drive_signature = [0; 4];
        match reader.read(&mut drive_signature) {
            Ok(4) => Ok(()),
            Ok(n_bytes_read) => Err(MbrError::InputTooSmall {
                input_size: n_bytes_read,
                error_location: "drive signature",
            }),
            Err(e) => Err(MbrError::IoError(e.to_string())),
        }?;

        let mut zero_buf = [0; 2];
        match reader.read(&mut zero_buf) {
            Ok(2) if zero_buf == [0, 0] => Ok(()),
            // Copy protected according to spec on wikipedia - we'll just ignore it
            Ok(2) if zero_buf == [0x5A, 0x5A] => Ok(()),
            Ok(2) => Err(MbrError::NullSectorIsNotNull { val: zero_buf }),
            Ok(n_bytes_read) => Err(MbrError::InputTooSmall {
                input_size: n_bytes_read,
                error_location: "null sector",
            }),
            Err(e) => Err(MbrError::IoError(e.to_string())),
        }?;

        let partition_table = MbrPartTable::try_from_reader(&mut reader)?;
        let mut bootsector_signature = [0; 2];

        match reader.read(&mut bootsector_signature) {
            Ok(2) => Ok(()),
            Ok(n_bytes_read) => Err(MbrError::InputTooSmall {
                input_size: n_bytes_read,
                error_location: "bootsector signature",
            }),
            Err(e) => Err(MbrError::IoError(e.to_string())),
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
    use std::{fs::File, io::Write};

    #[test]
    fn read_mbr() {
        let raspios_img = File::open("./raspios.img").unwrap();
        let mbr = Mbr::try_from_reader(raspios_img).unwrap();
        dbg!(mbr);
        let partuuid = format!("{:x}", u32::from_le_bytes(mbr.drive_signature));
        println!("PARTUUID: {}", partuuid);
    }

    #[test]
    fn read_and_write() {
        let raspios_img = File::open("./raspios.img").unwrap();
        let mut mbr = Mbr::try_from_reader(raspios_img).unwrap();
        mbr.drive_signature = 0x090b3d33_u32.to_le_bytes();
        mbr.partition_table.entries[2] = Some(
            PartInfo::try_from_lba(
                true,
                mbr.partition_table.entries[1].unwrap().end_sector_lba(),
                1024,
                PartType::btrfs(),
            )
            .unwrap(),
        );
        let mut out_file = File::create("./out.img").unwrap();
        let buf = <[u8; 512]>::try_from(&mbr).unwrap();
        out_file.write_all(&buf).unwrap();
    }

    /*
    #[test]
    fn new_image() {
        let raspios_img = File::open("./raspios.img").unwrap();
        let mut mbr = Mbr::try_from_reader(raspios_img).unwrap();
        mbr.drive_signature = 0x191b3d33_u32.to_le_bytes();

        let mut out_file = File::create("./out.img").unwrap();
        let buf = <[u8; 512]>::try_from(&mbr).unwrap();
        out_file.write_all(&buf).unwrap();
    }
    */

    #[test]
    fn chs_entry_inv() {
        let e1 = ChsEntry { raw: [6, 4, 33] };
        let (c, h, s) = e1.chs();
        let e2 = ChsEntry::try_from_chs(c, h, s);
        assert_eq!(Some(e1), e2);

        let e1 = ChsEntry { raw: [127, 42, 33] };
        let (c, h, s) = e1.chs();
        let e2 = ChsEntry::try_from_chs(c, h, s);
        assert_eq!(Some(e1), e2);

        let e1 = ChsEntry {
            raw: [42, 137, 0b11001010],
        };
        let (c, h, s) = e1.chs();
        let e2 = ChsEntry::try_from_chs(c, h, s);
        assert_eq!(Some(e1), e2);
    }

    #[test]
    fn lba_to_chs() {
        assert_eq!(
            ChsEntry::try_from_lba(8192),
            Ok(ChsEntry::try_from_chs(64, 0, 1).unwrap())
        );
        assert_eq!(
            ChsEntry::try_from_lba(532479),
            Err(MbrError::InvalidAddressChs {
                cylinder: 4159,
                head: 3,
                sector: 32
            })
        );
        assert_eq!(
            ChsEntry::try_from_lba(532480),
            Err(MbrError::InvalidAddressChs {
                cylinder: 4160,
                head: 0,
                sector: 1
            })
        );
        assert_eq!(
            ChsEntry::try_from_lba(3842047),
            Err(MbrError::InvalidAddressChs {
                cylinder: 30015,
                head: 3,
                sector: 32
            })
        );
    }

    #[test]
    fn chs_from_lba() {
        assert_eq!(
            PartInfo::try_from_lba(
                false,
                8192,
                524288,
                PartType::Fat32 {
                    visible: true,
                    scheme: AddrScheme::Lba,
                }
            )
            .unwrap(),
            PartInfo {
                bootable: false,
                first_sector_chs: ChsEntry { raw: [0, 1, 64,] },
                part_type: PartType::Fat32 {
                    visible: true,
                    scheme: AddrScheme::Lba,
                },
                last_sector_chs: ChsEntry { raw: [3, 32, 63,] },
                start_sector_lba: 8192,
                sector_count_lba: 524288,
            }
        );
    }
}

#[derive(Error, Debug, PartialEq, Eq, Hash)]
pub enum MbrError {
    #[error("can't create partition type from value {part_id:#X}. If you think this value should be valid please open an issue.")]
    UnknownPartitionType { part_id: u8 },
    #[error("an MBR is 512 bytes long, however the input size was only {input_size} bytes long. Couldn't read {error_location}")]
    InputTooSmall {
        input_size: usize,
        error_location: &'static str,
    },
    #[error("the null sector in the provided MBR should be identically 0 or contain 0x5A 0x5A, but it contains {:#X} {:#X}", val[0], val[1])]
    NullSectorIsNotNull { val: [u8; 2] },
    #[error("IO error: {0}")]
    IoError(String),
    #[error("a valid MBR should contain a bootsector signature of 0x55, 0xAA at addresses 0x01FE, 0x01FF, but it contains {:#X} {:#X}", sig[0], sig[1])]
    BootloaderSignatureNotSet { sig: [u8; 2] },
    #[error("too many missing fields in input, can't deduce remaining fields")]
    IncompleteInput,
    #[error("a MBR-based CHS partition can't support CHS address ({cylinder}, {head}, {sector}). Consider switching to LBA or making the partition smaller.")]
    InvalidAddressChs {
        cylinder: u32,
        head: u32,
        sector: u32,
    },
}
