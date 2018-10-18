#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_json;

use std::fs::File;
use std::io::Write;
use std::io::Read;

mod huffman;
mod bitstream;
mod bufferedwriter;
mod deflate;

fn main() {
	let filename = "/Users/livingon/Desktop/tar_room/host.log.tar.gz";
	let file = File::open(filename).expect("file not found");
	let mut output = File::create("/Users/livingon/Desktop/tar_room/rust.tar").expect("Could not create output");
	let mut read_source = ReadSource::new(file);
	let mut gzip = GZipReader::new(&mut read_source);
	let mut tar = TarReader {
		input: &mut gzip,
	};

	let mut bytes_written : usize = 0;
	loop {
		// match tar.next() {
		// 	Some(entry) => println!("Created {:?}", entry),
		// 	None => panic!("Finished parsing"),
		// }
		bytes_written += 1;
		if bytes_written == 326528 {
			println!("Whaooooo");
		}
		println!("aslkdjf : {}", bytes_written);
		output.write(&vec![gzip.next()][..]);
		// println!("Emission: {}", gzip.next());
	}
}


/// We're trying to build an on-demand pipeline of bits between all
/// the different formats of data (gzip, DEFLATE, tar).
///
/// This abstraction allows us to "pipe" data between the different
/// layers of storage but reverses the flow from the source pushing
/// data to the destination requesting data.  This allows us to
/// measure things like:
///
/// How many bytes of the raw file were needed to encode this real file?
/// How many back references existed in the last 128 bytes?
/// How many of the last 128 bytes were compressed versus literals?
pub trait Source<T> {
	fn next(&mut self) -> T;
	fn next_n(&mut self, n: usize) -> Vec<T> {
		let mut result = Vec::new();
		for _ in 0..n {
			result.push(self.next());
		}
		result
	}
}

/// Keeps track of how far we are through a gzip archive at the abstraction
/// level of "entries".  Also allows us to keep track of how far we are into
/// the gzip archive which allows us to report "how many bytes were needed for
/// a specific result"
struct GZipReader<'a> {
	deflater: deflate::DEFLATEReader<'a>,
}

/// The constant value of CM (Compression Method) header field as defined in
/// RFC 1952.  This is the only compression method supported.
const CM_DEFLATE : u8 = 8;

/// Bit mask to extract or set the "text" flag
const _FLAG_TEXT : u8 = 0b00000001;

/// Bit mask to extract or set the "HCRC" flag
const FLAG_HCRC : u8 = 0b00000010;

/// Bit mask to extract or set the "extra" flag
const FLAG_EXTRA : u8 = 0b00000100;

/// Bit mask to extract or set the "name" flag
const FLAG_NAME : u8 = 0b00001000;

/// Bit mask to extract or set the "comment" flag
const FLAG_COMMENT : u8 = 0b00010000;

impl <'a> GZipReader<'a> {

	fn new(input: &'a mut ReadSource) -> GZipReader<'a> {
		GZipReader::handle_headers(input);
		GZipReader{
			deflater: deflate::DEFLATEReader::new(input),
		}
	}

	/// Processes an input stream as a gzip compressed file
	fn handle_headers(input: &mut ReadSource) {
		let id1 = input.next();
		assert_eq!(id1, 0x1f);
		assert_eq!(id1, 0o037);
		assert_eq!(id1, 31);
		println!("ID1 is correct");

		let id2 = input.next();
		assert_eq!(id2, 0x8b);
		assert_eq!(id2, 0o213);
		assert_eq!(id2, 139);
		println!("ID2 is correct");

		let compression_method = input.next();
		assert_eq!(compression_method, CM_DEFLATE);
		println!("CM is correct");

		let _flags = input.next();
		println!("Flags: {:02x?}", _flags);

		let mut _mtime = vec![0; 4];
		input.fill_bytes(&mut _mtime);
		let _xfl = input.next();
		let _os = input.next();

		// At this point in the file, there can be extra fields.  I don't want to
		// implement that yet so I'm just going to assert that there are no extra
		// fields.
		assert_eq!(_flags & FLAG_EXTRA, 0);
		assert_eq!(_flags & FLAG_NAME, 0);
		assert_eq!(_flags & FLAG_COMMENT, 0);
		assert_eq!(_flags & FLAG_HCRC, 0);
	}
}

/// GZipReader is a source of bytes.  It produces
/// bytes for the DEFLATEReader to consume
impl <'a> Source<u8> for GZipReader<'a> {
	fn next(&mut self) -> u8 {
		match self.deflater.next() {
			Some(byte) => byte,
			None => unimplemented!(),
		}
	}
}


struct ReadSource<'a> {
	read : Box<Read + 'a>,
}

impl <'a> ReadSource<'a> {
	fn new<T: Read + 'a>(source: T) -> ReadSource<'a> {
		ReadSource{
			read: Box::new(source),
		}
	}

	fn fill_bytes(&mut self, bytes: &mut Vec<u8>) {
		match self.read.read(&mut bytes[..]) {
			Ok(bytes_read) if bytes_read == bytes.len() => {
				println!("Successfully read {:02x?}", bytes);
			},
			Ok(bytes_read) => panic!("Only read {} bytes", bytes_read),
			Err(err) => panic!("Error: {}", err),
		}
	}
}

impl <'a> Source<u8> for ReadSource<'a> {
	fn next(&mut self) -> u8 {
		let mut bytes : Vec<u8> = vec![0; 1];
		match self.read.read(&mut bytes[..]) {
			Ok(1) => {
				let byte = bytes[0];
				println!("Successfully read {:02x?}", byte);
				byte
			},
			Ok(bytes_read) => panic!("Only read {} bytes", bytes_read),
			Err(err) => panic!("Error: {}", err),
		}
	}
}

struct TarReader<'a> {
	input: &'a mut Source<u8>,
}

impl <'a> Source<Option<TarFileEntry>> for TarReader<'a> {
	fn next(&mut self) -> Option<TarFileEntry> {
		TarFileEntry::new(self.input)
	}
}

/// A single entry of a file in a tar archive.
#[derive(Debug)]
struct TarFileEntry {
	name: Box<String>,
	mode: Box<String>,
	uid: Box<String>,
	gid: Box<String>,
	size: Box<String>,
	mtime: Box<String>,
	chksum: Box<String>,
	typeflag: Box<String>,
	linkname: Box<String>,
	magic: Box<String>,
	version: Box<String>,
	uname: Box<String>,
	gname: Box<String>,
	devmajor: Box<String>,
	devminor: Box<String>,
	prefix: Box<String>,
}

impl TarFileEntry {
	fn new(input: &mut Source<u8>) -> Option<TarFileEntry> {
		let result = TarFileEntry {
			name: Box::new(String::from_utf8(input.next_n(100)).unwrap()),
			mode: Box::new(String::from_utf8(input.next_n(8)).unwrap()),
			uid: Box::new(String::from_utf8(input.next_n(8)).unwrap()),
			gid: Box::new(String::from_utf8(input.next_n(8)).unwrap()),
			size: Box::new(String::from_utf8(input.next_n(12)).unwrap()),
			mtime: Box::new(String::from_utf8(input.next_n(12)).unwrap()),
			chksum: Box::new(String::from_utf8(input.next_n(8)).unwrap()),
			typeflag: Box::new(String::from_utf8(input.next_n(1)).unwrap()),
			linkname: Box::new(String::from_utf8(input.next_n(100)).unwrap()),
			magic: Box::new(String::from_utf8(input.next_n(6)).unwrap()),
			version: Box::new(String::from_utf8(input.next_n(2)).unwrap()),
			uname: Box::new(String::from_utf8(input.next_n(32)).unwrap()),
			gname: Box::new(String::from_utf8(input.next_n(32)).unwrap()),
			devmajor: Box::new(String::from_utf8(input.next_n(8)).unwrap()),
			devminor: Box::new(String::from_utf8(input.next_n(8)).unwrap()),
			prefix: Box::new(String::from_utf8(input.next_n(155)).unwrap()),
		};

		// So far we've read 500 bytes, but since tar writes in 512 blocks,
		// we need to consume 12 more blocks to move to the next block
		let _ = input.next_n(12);


		if result.magic.as_str() == "\0\0\0\0\0\0" {
			assert_eq!(input.next_n(512), vec![0; 512]);

			return None
		}

		println!("Parsed headers: {:?}", result);

		assert_eq!(result.magic.as_str(), "ustar\0");
		assert_eq!(result.version.as_str(), "00");

		// Read the content of the file
		println!("Parsing '{}'", result.size);
		let size = &result.size[0..11];
		let bytes = u32::from_str_radix(size, 8).unwrap() as usize;
		println!("Reading contents! {}", bytes);
		let _content = input.next_n(bytes);
		println!("Read contents! {:?}", _content);
		// assert_eq!(checksum.into_bytes()[..], result.chksum);

		// Clean off enough bytes to get us to a 512 block size
		let remainder = match bytes % 512 {
			0 => 0,
			x => 512 - x,
		};
		println!("Finishing off {} bytes", remainder);
		input.next_n(remainder);

		Some(result)
	}
}
