package main;

import jp.ac.nagoya_u.is.ss.kishii.usui.system.game.AbstractPlayer;
import jp.ac.nagoya_u.is.ss.kishii.usui.system.game.CppPlayer;
import jp.ac.nagoya_u.is.ss.kishii.usui.system.game.PuyoPuyo;
import SamplePlayer.Nohoho;
import SamplePlayer.RandomPlayer;
import SamplePlayer.SamplePlayer;
import org.nisshiee.puyo.player.NisshieePlayer;

/**
 * メインクラス
 */

public class MainClass {

	public static void main(String args[]) {
		/*
		 * プレイヤーの読み込み
		 */
		AbstractPlayer player1 = new NisshieePlayer("Nisshiee");
		AbstractPlayer player2 = new RandomPlayer("Random");

		/*
		 * C++で作成したプレイヤーの読み込み
		 * まだ未実装
		 */
		//AbstractPlayer cppPlayer1 = new CppPlayer("test1", "PuyoPuyoPlayer");
		//AbstractPlayer cppPlayer2 = new CppPlayer("test2", "PuyoPuyoPlayer2");

		/*
		 * ゲームを実行
		 * 自前の画像を使用したい場合はディレクトリ名をコンストラクタに渡してください。
		 */
		PuyoPuyo puyopuyo = new PuyoPuyo(player1, player2);

		/*
		 * こちらは一人用
		 */
		//PuyoPuyo puyopuyo = new PuyoPuyo(player1);

		puyopuyo.puyoPuyo();
	}
}
