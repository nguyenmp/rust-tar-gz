
#[derive(Eq, PartialEq, Serialize)]
pub enum Huffman<T : Copy + Eq> {
	Branch{
		zero: Box<Huffman<T>>,
		one: Box<Huffman<T>>,
	},
	Leaf(T),
	DeadEnd,
}

impl <T: Copy + Eq + serde::ser::Serialize> std::fmt::Debug for Huffman<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}


impl <T : Copy + Eq + Ord> Huffman<T> {
	pub fn new(alphabet: &Vec<T>, bit_lengths: &Vec<usize>) -> Huffman<T> {
		assert_eq!(alphabet.len(), bit_lengths.len());

		let counts_by_length = count_by_code_length(bit_lengths);
		println!("Counts By Length: {:?}", counts_by_length);
		let max = get_max_code_length(bit_lengths);
		let starting_values = determine_starting_values(&counts_by_length, max);
		println!("Starting Values: {:?}", starting_values);
		let assigned_values = assign_values(alphabet, bit_lengths, &starting_values, max);
		println!("Assigned Values: {:?}", assigned_values);
		build_tree(alphabet, &assigned_values, bit_lengths)
	}
}

fn count_by_code_length(bit_lengths: &Vec<usize>) -> Vec<usize> {
	// Since we are counting the number of codes that have the same code length
	// The result only needs enough space to store up to the maximum code length
	// The value of this result map will be the number of codes that have that
	// the input length, which is the key/index to this array
	let max_code_length = get_max_code_length(bit_lengths);
	let mut count_by_code_length = vec![0; max_code_length + 1];
	for bit_length in bit_lengths {
		count_by_code_length[*bit_length] += 1;
	}
	count_by_code_length
}

fn get_max_code_length(bit_lengths: &Vec<usize>) -> usize {
	let mut max = 0;
	for bit_length in bit_lengths {
		if *bit_length > max {
			max = *bit_length;
		}
	}
	max
}

fn determine_starting_values(count_by_code_length: &Vec<usize>, max_code_length: usize) -> Vec<usize> {
	// We always start with an array of 0 because we start counting at length=1
	let mut starting_values = Vec::new();
	starting_values.push(0);

	for length in 1..count_by_code_length.len() {
		let last_value = starting_values[length - 1];
		let last_count = if length == 1 { 0 } else { count_by_code_length[length - 1] };
		starting_values.push((last_value + last_count) << 1);
		println!("Previous Value: {}, Previous Count: {}, New Value: {}", last_value, last_count, starting_values.last().unwrap());
	}
	starting_values
}

fn assign_values<T : Copy + Ord>(alphabet: &Vec<T>, bit_lengths: &Vec<usize>, starting_values: &Vec<usize>, max_length: usize) -> Vec<usize> {
	println!("Assigning real values given these lengths: {:?} and starting_values: {:?}", bit_lengths, starting_values);
	let mut values = vec![0; bit_lengths.len()];
	let index_order = get_index_ordering(alphabet);


	// We need max_length + 1 because we're using that value as an index
	// into an array.  Otherwise, we'd never actually look at max_length
	// since ranges are not inclusive on that end
	for current_length in 1..(max_length + 1) {
		let mut starting_value = starting_values[current_length];
		for index in &index_order {
			if bit_lengths[*index] == current_length {
				values[*index] = starting_value;
				starting_value += 1;
			}
		}
	}
	values
}

/// This inverts the code length huffman tree's alphabet
/// Huffman trees work by assigning value to the lowest lexical
/// value first, but the alphabet is not in lexical order
/// So we need to figure out how to invert the alphabet to get
/// the order of indexes to walk through
fn get_index_ordering<T : Ord + Copy>(alphabet: &Vec<T>) -> Vec<usize> {
	// alphabet is givne_index -> value
	// sorted is walking_index -> value
	// result is walking_index -> given_index
	let mut sorted = alphabet.clone();
	sorted.sort_unstable();
	let mut result = vec![0; alphabet.len()];
	for given_index in 0..alphabet.len() {
		let value = alphabet[given_index];
		let walking_index = sorted.binary_search(&value).expect("Alphabet inversion resulted in not finding a value");
		result[walking_index] = given_index;
	}
	println!("Walking order: {:?}", result);
	result
}

fn build_tree<T : Copy + Eq>(alphabet: &Vec<T>, values: &Vec<usize>, lengths: &Vec<usize>) -> Huffman<T> {
	let depth = 0;
	let mut indexes = Vec::new();
	for index in 0..lengths.len() {
		let length = lengths[index];
		if length > 0 {
			indexes.push(index)
		}
	}
	println!("Only considering {:?}", indexes);
	_build_tree(alphabet, values, depth, lengths, &indexes)
}

fn _build_tree<T : Copy + Eq>(alphabet: &Vec<T>, values: &Vec<usize>, depth: usize, lengths: &Vec<usize>, indexes: &Vec<usize>) -> Huffman<T> {
	// If we only have one index to consider, then we are at the leaf
	// and the value of that leaf is the alphabet at that index
	if indexes.len() == 1 {
		println!("Leafing this alone!");
		return Huffman::Leaf(alphabet[indexes[0].clone()])
	} else if indexes.len() == 0 {
		println!("This is a dead end, and that's okay");
		return Huffman::DeadEnd
	}

	// Otherwise, we need to loop through our options and
	// descriminate between the ones and zeros
	// Group the considerations by the current bit value = one or = zero
	// this will parititon the alphabet into the left and right branches of the tree
	let mut zeros = Vec::new();
	let mut ones = Vec::new();
	for index in indexes {
		let length = lengths[*index];
		let value = values[*index];

		// If we're at depth 0, and we're looking at a value of length 3
		// Then the bit we are looking at is the 2 in [2, 1, 0]
		let bit = (value >> (length - 1 - depth)) & 0b1;
		println!("Index {:?}, Value {:06b}, length {}, depth {}, bit {:01b}", *index, value, length, depth, bit);
		if bit == 1 {
			ones.push(*index);
		} else if bit == 0 {
			zeros.push(*index);
		} else {
			panic!("bit was neither one nor zero");
		}
	}

	println!("Ones: {:?}, Zeros: {:?}", ones, zeros);

	Huffman::Branch {
		zero: Box::new(_build_tree(alphabet, values, depth + 1, lengths, &zeros)),
		one: Box::new(_build_tree(alphabet, values, depth + 1, lengths, &ones)),
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	#[test]
	fn create_huffman() {
		let alphabet = vec!['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
		let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
		let tree = Huffman::new(&alphabet, &bit_lengths);
		assert_eq!(
			tree,
			Huffman::Branch {
				zero: Box::new(Huffman::Branch {
					zero: Box::new(Huffman::Leaf('F')),
					one: Box::new(Huffman::Branch {
						zero: Box::new(Huffman::Leaf('A')),
						one: Box::new(Huffman::Leaf('B'))
					})
				}),
				one: Box::new(Huffman::Branch {
					zero: Box::new(Huffman::Branch {
						zero: Box::new(Huffman::Leaf('C')),
						one: Box::new(Huffman::Leaf('D'))
					}),
					one: Box::new(Huffman::Branch {
						zero: Box::new(Huffman::Leaf('E')),
						one: Box::new(Huffman::Branch {
							zero: Box::new(Huffman::Leaf('G')),
							one: Box::new(Huffman::Leaf('H'))
						})
					})
				})
			}
		)
	}

	#[test]
	fn test_determine_starting_values() {
		let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
		let counts_by_code_length = count_by_code_length(&bit_lengths);
		let max = get_max_code_length(&bit_lengths);
		let starting_values = determine_starting_values(&counts_by_code_length, max);
		assert_eq!(starting_values, vec![0, 0, 0, 2, 14]);
	}

	#[test]
	fn test_assign_values() {
		let alphabet = vec!['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
		let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
		let counts_by_code_length = count_by_code_length(&bit_lengths);
		let max = get_max_code_length(&bit_lengths);
		let starting_values = determine_starting_values(&counts_by_code_length, max);
		let assigned_values = assign_values(&alphabet, &bit_lengths, &starting_values, max);
		assert_eq!(assigned_values, vec![0b010, 0b011, 0b100, 0b101, 0b110, 0b00, 0b1110, 0b1111]);
	}

	#[test]
	fn test_count_by_code_length() {
		let bit_lengths = vec![3, 3, 3, 3, 3, 2, 4, 4];
		let result = count_by_code_length(&bit_lengths);
		assert_eq!(result, vec![0, 0, 1, 5, 2]);
	}
}