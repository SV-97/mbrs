use std::io::Read;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RawMbr {
    buf: [u8; 512],
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum MbrProblem {
    NullSectorIsNotNull,
    BootloaderSignatureNotSet,
}

impl RawMbr {
    pub fn bootloader_mut(&mut self) -> &mut [u8; 440] {
        (&mut self.buf[0..440]).try_into().unwrap()
    }

    pub fn drive_signature_mut(&mut self) -> &mut [u8; 4] {
        (&mut self.buf[440..444]).try_into().unwrap()
    }

    pub fn null_sector_mut(&mut self) -> &mut [u8; 2] {
        (&mut self.buf[444..446]).try_into().unwrap()
    }

    pub fn partition_table_mut(&mut self) -> &mut [u8; 64] {
        (&mut self.buf[446..510]).try_into().unwrap()
    }

    pub fn bootsector_signature_mut(&mut self) -> &mut [u8; 2] {
        (&mut self.buf[510..511]).try_into().unwrap()
    }

    pub fn bootloader(&self) -> &[u8; 440] {
        self.buf[0..440].try_into().unwrap()
    }

    pub fn drive_signature(&self) -> &[u8; 4] {
        self.buf[440..444].try_into().unwrap()
    }

    pub fn null_sector(&self) -> &[u8; 2] {
        self.buf[444..446].try_into().unwrap()
    }

    pub fn partition_table(&self) -> MbrPartTable {
        MbrPartTable {
            buf: self.buf[446..510].try_into().unwrap(),
        }
    }

    pub fn bootsector_signature(&self) -> &[u8; 2] {
        self.buf[510..512].try_into().unwrap()
    }

    pub fn try_from_reader<B>(mut reader: B) -> Result<Self, std::io::Error>
    where
        B: Read,
    {
        use std::io::{Error, ErrorKind};
        let mut buf = [0; 512];
        match reader.read(&mut buf) {
            Ok(512) => Ok(Self { buf }),
            Ok(n_bytes_read) => Err(Error::new(
                ErrorKind::UnexpectedEof,
                format!("Reader could not supply the requires 512 bytes for an MBR. Could only read {} bytes.", n_bytes_read),
            )),
            Err(e) => Err(e),
        }
    }

    pub fn validate(&self) -> Option<MbrProblem> {
        if self.null_sector() != &[0, 0] {
            Some(MbrProblem::NullSectorIsNotNull)
        } else if self.bootsector_signature() != &[0x55, 0xAA] {
            Some(MbrProblem::BootloaderSignatureNotSet)
        } else {
            None
        }
    }

    pub fn set_bootsector_sig(&mut self) {
        *self.bootsector_signature_mut() = [0x55, 0xAA];
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct MbrPartTable<'a> {
    buf: &'a [u8; 64],
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct PartInfo<'a> {
    // all entries are stored in little endian format
    // unused entries should be zeroed
    buf: &'a [u8; 16],
}

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

impl<'a> PartInfo<'a> {
    pub fn bootable(&self) -> Result<bool, u8> {
        match self.buf[0] {
            0x80 => Ok(true),
            0x00 => Ok(false),
            n => Err(n),
        }
    }

    /// CHS entry of first sector
    pub fn first_sector_chs(&self) -> &[u8; 3] {
        self.buf[1..4].try_into().unwrap()
    }

    pub fn part_typ(&self) -> Result<PartType, ()> {
        PartType::try_from(self.buf[4])
    }

    /// CHS entry of last sector
    pub fn last_sector_chs(&self) -> &[u8; 3] {
        self.buf[5..8].try_into().unwrap()
    }

    pub fn start_sector_lba(&self) -> &[u8; 4] {
        self.buf[8..12].try_into().unwrap()
    }

    pub fn sector_count_lba(&self) -> &[u8; 4] {
        self.buf[12..16].try_into().unwrap()
    }
}

impl<'a> MbrPartTable<'a> {
    /// Not all entries might be valid
    /// TODO: parse entries directly and return options
    pub fn entries(&self) -> [PartInfo; 4] {
        [
            PartInfo {
                buf: self.buf[0..16].try_into().unwrap(),
            },
            PartInfo {
                buf: self.buf[16..2 * 16].try_into().unwrap(),
            },
            PartInfo {
                buf: self.buf[2 * 16..3 * 16].try_into().unwrap(),
            },
            PartInfo {
                buf: self.buf[3 * 16..64].try_into().unwrap(),
            },
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;

    #[test]
    fn read_mbr() {
        let raspios_img = File::open("./raspios.img").unwrap();
        let mbr = RawMbr::try_from_reader(raspios_img).unwrap();
        dbg!(mbr.bootloader());
        dbg!(mbr.validate());
        dbg!(mbr.partition_table().entries()[0].part_typ());
        dbg!(mbr.partition_table().entries()[1].part_typ());
        panic!()
    }
}
