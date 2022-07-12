//package de.tuda.stg.consys.demo.invariantdemos.schema.tournament;
//
//import de.tuda.stg.consys.demo.invariantdemos.Schema;
//import de.tuda.stg.consys.japi.legacy.JRef;
//
//import java.util.HashSet;
//import java.util.Random;
//import java.util.Set;
//
//public class TournamentsSchema extends Schema<Tournaments> {
//	private final Random random = new Random();
//
//	private Set<Player> localPlayers = new HashSet();
//	private Set<Tournament> localTournaments = new HashSet();
//
//	@Override
//	public Tournaments newInstance() {
//		return new Tournaments();
//	}
//
//	@Override
//	public Class<Tournaments> instanceClass() {
//		return Tournaments.class;
//	}
//
//	@Override
//	public void doOperation(JRef<Tournaments> ref) {
//		int rand = random.nextInt(100);
//		if (rand < 20) {
//			Player p = new Player("Thorsten");
//			localPlayers.add(p);
//			ref.invoke("addPlayer", p);
//		} else if (rand < 40) {
//			Tournament t = new Tournament();
//			localTournaments.add(t);
//			ref.invoke("addTournament", t);
//		} else if (rand < 60) {
//			if (localTournaments.isEmpty()) {
//				Tournament t = new Tournament();
//				localTournaments.add(t);
//				ref.invoke("addTournament", t);
//			} else {
//				ref.invoke("beginTournament", localTournaments.iterator().next());
//			}
//		} else if (rand < 80) {
//			if (localTournaments.isEmpty()) {
//				Tournament t = new Tournament();
//				localTournaments.add(t);
//				ref.invoke("addTournament", t);
//			} else if (localPlayers.isEmpty()) {
//				Player p = new Player("Thorsten");
//				localPlayers.add(p);
//				ref.invoke("addPlayer", p);
//			} else {
//				ref.invoke("enrollTournament", localPlayers.iterator().next(), localTournaments.iterator().next());
//			}
//		}  else if (rand < 100) {
//			if (localPlayers.isEmpty()) {
//				Player p = new Player("Thorsten");
//				localPlayers.add(p);
//				ref.invoke("addPlayer", p);
//			} else {
//				ref.invoke("addFunds", localPlayers.iterator().next(), 1);
//			}
//		}
//	}
//}
