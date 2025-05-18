use rand::Rng;

pub fn chars(length: usize) -> String {
    (0..length)
        .map(|_| {
            let range = if rand::random() { b'a'..=b'z' } else { b'A'..=b'Z' };
            rand::rng().random_range(range) as char
        })
        .collect()
}