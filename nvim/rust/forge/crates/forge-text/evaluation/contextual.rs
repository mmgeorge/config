use std::{collections::HashMap, error::Error, fs, path::Path, time::Instant};
use symspell_rs::{SymSpell, Verbosity};

/// Compare every lexical candidate with a smoothed bidirectional bigram score.
fn main() -> Result<(), Box<dyn Error>> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("data");
    let mut dictionary = SymSpell::new(2, None, 7, 1);
    let mut unigram = HashMap::new();
    let mut total = 0.0;
    for line in fs::read_to_string(root.join("frequency_dictionary_en_82_765.txt"))?.lines() {
        let mut fields = line.split_whitespace();
        let word = fields.next().ok_or("word missing")?;
        let count: usize = fields.next().ok_or("count missing")?.parse()?;
        dictionary.create_dictionary_entry(word, count);
        unigram.insert(word.to_owned(), count as f64);
        total += count as f64;
    }
    let mut bigram = HashMap::new();
    let mut predecessor = HashMap::new();
    for line in fs::read_to_string(root.join("frequency_bigramdictionary_en_243_342.txt"))?.lines() {
        let fields: Vec<_> = line.split_whitespace().collect();
        if fields.len() != 3 { return Err("bad bigram".into()); }
        let count: f64 = fields[2].parse()?;
        bigram.insert((fields[0].to_owned(), fields[1].to_owned()), count);
        *predecessor.entry(fields[0].to_owned()).or_insert(0.0) += count;
    }
    let penalty = std::env::args().nth(1).unwrap_or("4.0".into()).parse::<f64>()?;
    let development = [
      ("Please recieve the package", "recieve", "receive"),
      ("The update should releive pressure", "releive", "relieve"),
      ("Please receive the package", "receive", "receive"),
      ("The update should relieve pressure", "relieve", "relieve"),
      ("Read the response form the server", "form", "from"),
      ("Fill out the form before submitting", "form", "form"),
      ("The value is greater then zero", "then", "than"),
      ("Save the file then exit", "then", "then"),
      ("Do not loose your work", "loose", "lose"),
      ("The cable is loose", "loose", "loose"),
      ("This should definately fix the issue", "definately", "definitely"),
      ("We need to seperate configuration from state", "seperate", "separate"),
      ("The problem occured during startup", "occured", "occurred"),
      ("Check the dependancies before building", "dependancies", "dependencies"),
      ("The service recieved a request", "recieved", "received"),
      ("Please recieve", "recieve", "receive"),
      ("The value is greater then", "then", "than"),
      ("Fill out the form", "form", "form"),
    ];
    let heldout = [
      ("We recieve messages every day", "recieve", "receive"),
      ("This medicine can releive pain", "releive", "relieve"),
      ("Download the file form the website", "form", "from"),
      ("The threshold is less then ten", "then", "than"),
      ("You may loose the connection", "loose", "lose"),
      ("Keep the two values seperate", "seperate", "separate"),
      ("That definately solves the problem", "definately", "definitely"),
      ("An error occured while saving", "occured", "occurred"),
      ("They recieved your message", "recieved", "received"),
      ("Update the dependancies first", "dependancies", "dependencies"),
      ("Please read teh instructions", "teh", "the"),
      ("The adress is incorrect", "adress", "address"),
      ("We need to recieve", "recieve", "receive"),
      ("A value greater then", "then", "than"),
      ("The request was sucessful", "sucessful", "successful"),
      ("Please accomodate the change", "accomodate", "accommodate"),
      ("We receive messages every day", "receive", "receive"),
      ("This medicine can relieve pain", "relieve", "relieve"),
      ("Complete the form on the website", "form", "form"),
      ("Save your work then exit", "then", "then"),
      ("The connection is loose", "loose", "loose"),
      ("Keep the two values separate", "separate", "separate"),
      ("That definitely solves the problem", "definitely", "definitely"),
      ("An error occurred while saving", "occurred", "occurred"),
      ("They received your message", "received", "received"),
      ("Update the dependencies first", "dependencies", "dependencies"),
      ("Please read the instructions", "the", "the"),
      ("The address is incorrect", "address", "address"),
      ("We need to receive", "receive", "receive"),
      ("A value greater than", "than", "than"),
      ("The request was successful", "successful", "successful"),
      ("Please accommodate the change", "accommodate", "accommodate"),
    ];
    let fixture = if std::env::args().nth(2).as_deref() == Some("heldout") { &heldout[..] } else { &development[..] };
    let started = Instant::now();
    let mut failures=0;
    for &(sentence, target, expected) in fixture {
      let words: Vec<_> = sentence.to_lowercase().split_whitespace().map(str::to_owned).collect();
      let position = words.iter().position(|word| word == target).ok_or("target missing")?;
      let mut candidates = dictionary.lookup(target, Verbosity::All, 2, &None, None, false);
      if !candidates.iter().any(|candidate| candidate.term == target) {
        candidates.push(symspell_rs::Suggestion { term: target.to_owned(), distance: 0, count: 0 });
      }
      let probability = |word: &str| (unigram.get(word).copied().unwrap_or(1.0)+1.0)/total;
      let conditional = |left: &str,right: &str| {
        let count = bigram.get(&(left.to_owned(),right.to_owned())).copied().unwrap_or(0.0);
        let previous = predecessor.get(left).copied().unwrap_or(0.0);
        0.5 * count/(previous.max(1.0)) + 0.5 * probability(right)
      };
      let score = |word: &str, distance: usize| {
        let context = if position>0 {conditional(&words[position-1], word).ln()} else {0.3*probability(word).ln()}
          + if position+1<words.len(){conditional(word, &words[position+1]).ln()} else {0.0};
        let transposed = target.len()==word.len() && (0..target.len().saturating_sub(1)).any(|index| {
          let mut swapped=target.as_bytes().to_vec(); swapped.swap(index,index+1); swapped==word.as_bytes()
        });
        let vowel_change = target.len()==word.len() && target.bytes().zip(word.bytes()).filter(|(first,second)| first!=second).all(|(first,second)| b"aeiouy".contains(&first) && b"aeiouy".contains(&second));
        let channel = if transposed { 0.5 } else if target.len()!=word.len() { 2.0 } else if vowel_change {1.2} else {6.8};
        let prior = if word==target {0.0} else {(if unigram.contains_key(target) {penalty} else {0.0}) + channel + (distance.saturating_sub(1) as f64)*5.0};
        context-prior
      };
      candidates.sort_by(|first,second| score(&second.term,second.distance).total_cmp(&score(&first.term,first.distance)));
      let chosen=&candidates[0].term;
      if chosen!=expected {failures+=1;}
      println!("{} | {} -> {} expected {} | {:?}", if chosen==expected {"PASS"}else{"FAIL"},sentence,chosen,expected,candidates.iter().take(4).map(|candidate|(&candidate.term,score(&candidate.term,candidate.distance))).collect::<Vec<_>>());
    }
    println!("failures={failures} evaluation_ms={}",started.elapsed().as_millis());
    Ok(())
}
