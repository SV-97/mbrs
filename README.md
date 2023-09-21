# mbrs
Master boot record parsing, manipulation and binary export.

The [MBR](https://en.wikipedia.org/wiki/Master_boot_record) is a legacy boot sector format that was for example used on old windows systems and is still being used on raspberry pis.

I wrote this library for personal use because I was annoyed by using `parted` to work with raspberry pi images, so I mainly paid attention to that usecase. This means not all the different historical MBR structures are supported (for example disk timestamps) and some niche partition type identifiers might not be implemented. Currently we implement the MBR precisely as described on the German wikipedia page. If you encounter a need for other partition types, timestamps or whatever please file an issue (or PR) and I might look into it.

I don't see why you'd ever need it but this crate supports `no_std` - just disable the `std` feature that's enabled by default.

# Example

```rust
let raspios_img = File::open("./raspios.img").unwrap();
let mbr = Mbr::try_from_reader(raspios_img).unwrap();
// print it
dbg!(mbr);
// read out the drive signature
let partuuid = format!("{:x}", u32::from_le_bytes(mbr.drive_signature));
println!("PARTUUID: {}", partuuid);

// read the MBR off a raspberry pi image file
let raspios_img = File::open("./raspios.img").unwrap();
let mut mbr = Mbr::try_from_reader(raspios_img).unwrap();
// modify the drive signature
mbr.drive_signature = 0x090b3d33_u32.to_le_bytes();
// add a new BTRFS 1024 sector big partition 
mbr.partition_table.entries[2] = Some(
    PartInfo::try_from_lba(
        true,
        mbr.partition_table.entries[1].unwrap().end_sector_lba(),
        1024,
        PartType::btrfs(),
    )
    .unwrap(),
);
// write the modified MBR to a new file
let mut out_file = File::create("./out.img").unwrap();
let buf = <[u8; 512]>::try_from(&mbr).unwrap();
out_file.write_all(&buf).unwrap();
```
