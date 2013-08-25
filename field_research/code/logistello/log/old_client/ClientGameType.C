#include "ClientTypes.H"
#include "ClientGameType.H"

bool operator == (const ClientGameType &c1, const ClientGameType &c2)
{
  //  if (typeof(c1) != typeof(c2)) return false;
  if (!c1.equal_up_to_pref(c2)) return false;
  return c1.pref_color == c2.pref_color;
}


ClientGameType::ClientGameType()
{
  rand_type = -1;
  komi_game = false;
  anti_game = false;
  pref_color = ClientColor::UNDEF;
}


ClientGameType &ClientGameType::operator=(const ClientGameType &x)
{
  bt = x.bt->clone();
  rand_type  = x.rand_type;
  komi_game  = x.komi_game;
  anti_game  = x.anti_game;
  pref_color = x.pref_color;
  return *this;
}


  

String ClientGameType::parse(istream &is)
{
  rand_type = -1;
  anti_game = false;
  komi_game = false;
  pref_color = ClientColor::UNDEF;

  String es = bt->parse(is);
  if (!es.empty()) return es;

  char c;
  
  do {

    c = is.get();
    
    switch (toupper(c)) {

    case 'K':
      
      if (pref_color != ClientColor::UNDEF) {
	return "no preferred color in komi games";
      }
      komi_game = true;
      break;

    case 'R':
      if (pref_color != ClientColor::UNDEF) {
	return "no preferred color in rand games";
      }

      if (has_rand_type()) {
      
	is >> rand_type;
      
	if (!is) return "#rand type missing";
	if (rand_type < 0) return "#rand type < 0";
      }
      break;

    case 'A':

      if (!has_anti()) return "no anti mode";
      anti_game = true;
      break;

    case 'B':
    case 'W':

      if (!has_pref_color()) return "no preferred color";
      
      if (is_komi_game() || is_rand_game()) {
	return "no preferred color in komi or rand games";
      }
      if (pref_color != ClientColor::UNDEF) {
	return "only one color preference allowed";
      }

      if (toupper(c) == 'B')
	pref_color = ClientColor::BLACK;
      else
	pref_color = ClientColor::WHITE;
      break;
      
    default:
      break;
    }
  } while (is);

  if (!is.eof() && !isspace(c)) return "illegal game type";

  if (is_rand_game() && !is_komi_game())
    return "2-game random mode not yet implemented";
  
  return "";
}


String ClientGameType::to_string() const { return to_string(false, 0); } 

String ClientGameType::to_string_with_komi(real4 komi) const
{
  return to_string(true, komi);
}

String ClientGameType::to_string(bool with_komi, real4 komi) const
{
  String s = bt->to_string(), t;

  if (has_komi() && is_komi_game()) {
    s += "k"; if (with_komi) { t.form("%+.2f", komi); s += t; }
  }
  if (has_rand() && is_rand_game()) {
    s += "r";
    if (has_rand_type()) t.form("%d", rand_type); s += t;
  }

  if (has_pref_color()) {
    if (get_pref_color() == ClientColor::BLACK) s += "b";
    if (get_pref_color() == ClientColor::WHITE) s += "w";
  }
  if (has_anti() && is_anti_game()) s += "a";
  return s;
}


String ClientGameType::key() const
{
  String s = bt->to_string();
  if (has_rand() && is_rand_game()) s += "r";
  if (has_anti() && is_anti_game()) s += "a";
  return s;
}

bool ClientGameType::equal_up_to_pref(const ClientGameType &t) const {
  if (*bt != *t.bt) return false;
  if (has_rand_type() && rand_type != t.rand_type) return false;
  if (has_komi() && komi_game != t.komi_game)      return false;
  if (has_anti() && anti_game != t.anti_game)      return false;
  return true;
}


bool ClientGameType::is_matching(const ClientGameType &t) const
{
  if (!equal_up_to_pref(t)) return false;
  
  if (pref_color == ClientColor::UNDEF) 
    return t.pref_color == ClientColor::UNDEF;
  
  return ClientColor::opponent(pref_color) == t.pref_color;
}
