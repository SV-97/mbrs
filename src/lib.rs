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
    /// Note that only the 6 bottom bits of sector and 10 bottom bits of cylinder will be used
    pub fn new(cylinder: u16, head: u8, sector: u8) -> Self {
        ChsEntry {
            raw: [
                head,
                (sector & SECTOR_MASK) | (((cylinder & 0x300) >> 2) as u8),
                (cylinder & 0xFF) as u8,
            ],
        }
    }

    pub fn head(&self) -> u8 {
        self.raw[0]
    }

    pub fn sector(&self) -> u8 {
        self.raw[1] & SECTOR_MASK
    }

    pub fn cylinder(&self) -> u16 {
        (((self.raw[1] & !SECTOR_MASK) as u16) << 2) | (self.raw[2] as u16)
    }

    pub fn chs(&self) -> (u16, u8, u8) {
        (self.cylinder(), self.head(), self.sector())
    }

    pub fn from_lba(lba: u32) -> Self {
        let heads_per_cylinder: u32 = 255;
        let sectors_per_track: u32 = 63;
        let cylinder = lba / (heads_per_cylinder * sectors_per_track);
        let head = (lba / sectors_per_track) % heads_per_cylinder;
        let sector = lba % sectors_per_track + 1;
        ChsEntry::new(
            u16::try_from(cylinder).unwrap(),
            u8::try_from(head).unwrap(),
            u8::try_from(sector).unwrap(),
        )
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
    pub fn from_lba(
        bootable: bool,
        start_sector_lba: u32,
        sector_count_lba: u32,
        part_type: PartType,
    ) -> Self {
        Self {
            bootable,
            first_sector_chs: ChsEntry::from_lba(start_sector_lba),
            last_sector_chs: ChsEntry::from_lba(start_sector_lba + sector_count_lba),
            start_sector_lba,
            sector_count_lba,
            part_type,
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
    use std::{fs::File, io::Write};

    #[test]
    fn read_mbr() {
        let raspios_img = File::open("./raspios.img").unwrap();
        let mbr = Mbr::try_from_reader(raspios_img).unwrap();
        dbg!(mbr);
        let partuuid = format!("{:x}", u32::from_le_bytes(mbr.drive_signature));
        println!("PARTUUID: {}", partuuid);

        let e1 = ChsEntry { raw: [0, 1, 64] };
        let c: u32 = e1.cylinder() as u32;
        let h: u32 = e1.head() as u32;
        let s: u32 = e1.sector() as u32 - 1;
        dbg!((c, h, s));
        let lba = (c * 255 + h) * 63 + s - 1;
        dbg!(lba);
        // dbg!(ChsEntry::new(33, 4, 4));
        // dbg!((e1.cylinder(), e1.head(), e1.sector()));

        dbg!(PartInfo::from_lba(
            false,
            8192,
            524288,
            PartType::Fat32 {
                visible: true,
                scheme: AddrScheme::Lba,
            }
        ));
        dbg!(PartInfo::from_lba(
            false,
            8192,
            524288,
            PartType::Fat32 {
                visible: true,
                scheme: AddrScheme::Lba,
            }
        )
        .first_sector_chs
        .chs());

        let mut out_file = File::create("./out.img").unwrap();
        let buf = <[u8; 512]>::try_from(&mbr).unwrap();
        out_file.write_all(&buf).unwrap();
        panic!()
    }

    #[test]
    fn chs_entry_inv() {
        let e1 = ChsEntry { raw: [6, 4, 33] };

        let c = e1.cylinder();
        let h = e1.head();
        let s = e1.sector();

        let e2 = ChsEntry::new(c, h, s);
        assert_eq!(e1, e2);

        let e1 = ChsEntry { raw: [127, 42, 33] };

        let c = e1.cylinder();
        let h = e1.head();
        let s = e1.sector();

        let e2 = ChsEntry::new(c, h, s);
        assert_eq!(e1, e2);

        let e1 = ChsEntry {
            raw: [42, 137, 0b11001010],
        };

        let c = e1.cylinder();
        let h = e1.head();
        let s = e1.sector();

        let e2 = ChsEntry::new(c, h, s);
        assert_eq!(e1, e2);
    }

    #[test]
    #[ignore]
    fn chs_from_lba() {
        assert_eq!(
            PartInfo::from_lba(
                false,
                8192,
                524288,
                PartType::Fat32 {
                    visible: true,
                    scheme: AddrScheme::Lba,
                }
            ),
            PartInfo {
                bootable: false,
                first_sector_chs: ChsEntry { raw: [0, 1, 64,] },
                part_type: PartType::Fat32 {
                    visible: true,
                    scheme: AddrScheme::Lba,
                },
                last_sector_chs: ChsEntry {
                    raw: [3, 224, 255,],
                },
                start_sector_lba: 8192,
                sector_count_lba: 524288,
            }
        );
    }
}

/*
 mbr.partition_table = MbrPartTable {
            entries: [
                Some(PartInfo {
                    bootable: true,
                    first_sector_chs:
                }),
                None,
                None,
                None,
            ]
        };
*/

/*

        PartInfo {
            bootable: false,
            first_sector_chs: ChsEntry {
                raw: [
                    0,
                    1,
                    64,
                ],
            },
            part_type: Fat32 {
                visible: true,
                scheme: Lba,
            },
            last_sector_chs: ChsEntry {
                raw: [
                    3,
                    224,
                    255,
                ],
            },
            start_sector_lba: 8192,
            sector_count_lba: 524288,
        },
*/
