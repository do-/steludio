
	$_REQUEST {_label} or return "#_label#:Вы забыли ввести наименование";
#	$_REQUEST {_id_user} or return "#_id_user#:Вы забыли выбрать пользователя";

#	$_REQUEST {_id_org} = sql_select_id (orgs => {fake => 0, label => $_REQUEST {_id_org__label}},
#		['label'],
#		sub {vb_yes ("Организации '$_REQUEST{_id_org__label}' пока нет в справочнике. Добавить её прямо сейчас?")},
#	) or return '#_id_org#:Действие отменено';

	return undef;
	