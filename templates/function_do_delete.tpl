#	$item = sql_select_hash ('__TYPE__');

	sql_do ("UPDATE __TYPE__ SET fake = -1 WHERE id = ?", $_REQUEST [id]);
	esc ();
