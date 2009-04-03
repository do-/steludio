	my ($data) = @_;

	return

		draw_table (

			[
				{
					label  => ' ',
					hidden =>
						$_REQUEST {fake} eq '0,-1'
					,
				},
				'Наименование',
			],

			sub {

				__d ($i);

				draw_cells ({
					href => "/?type=__TYPE__&id=$$i{id}",
				},[
	
					{
						type   => 'checkbox',
						name   => "___TYPE___$i->{id}",
						hidden =>
							$_REQUEST {fake} eq '0,-1'
						,
#						off    =>
#							$_USER -> {id} != $i -> {id_user}
#							&& $_USER -> {role} ne 'admin'
#						,
					},

					$i -> {label},

				])

			},

			$data -> {__TYPE__},

			{
				
				name => 't1',
				
				title => {label => action_type_label},

#				path => [
#					{name => 'Главная страница', type => 'home_page', id => ''},
#					{name => action_type_label, type => '__TYPE__', id => ''},
#				],

				top_toolbar => [{
						keep_params => ['type', 'select'],
					},

					{
						icon  => 'create',
						label => '&Добавить',
						href  => '?type=__TYPE__&action=create',
					},


#					{
#						icon    => 'delete',
#						label   => 'Удалить',
#						href    => "javaScript:if (confirm('Вы уверены, что хотите удалить все выделенные ...?')) {var f = document.forms['t1']; f.elements['action'].value='kill'; f.submit()} void(0);",
#						confirm => '',
#						off     =>
#							$_REQUEST {fake}
#							,
#					},

#					{
#						icon    => 'restore',
#						label   => 'Восстановить',
#						href    => "javaScript:if (confirm('Вы уверены, что хотите восстановить все выделенные ...?')) {var f = document.forms['t1']; f.elements['action'].value='unkill'; f.submit()} void(0);",
#						confirm => '',
#						off     =>
#							$_REQUEST {fake} != -1
#							,
#					},


					{
						type  => 'input_text',
						label => 'Искать',
						name  => 'q',
						keep_params => [],
					},

					{
						type   => 'input_select',
						name   => 'id_...',
						values => $data -> {...},
						empty  => '[Все ...]',
					},

					{
						type    => 'pager',
					},

					fake_select (),

				],
				
				toolbar => draw_centered_toolbar ({},[

					{
						icon  => 'create',
						label => 'Добавить',
						href  => '?type=__TYPE__&action=create',
						keep_esc => 1,
					},

					{
						icon    => 'delete',
						label   => 'Удалить',
						href    => "javaScript:if (confirm('Вы уверены, что хотите удалить все выделенные ...?')) {var f = document.forms['t1']; f.elements['action'].value='kill'; f.submit()} void(0);",
						confirm => '',
						off     =>
							$_REQUEST {fake}
							,
					},

					{
						icon    => 'restore',
						label   => 'Восстановить',
						href    => "javaScript:if (confirm('Вы уверены, что хотите восстановить все выделенные ...?')) {var f = document.forms['t1']; f.elements['action'].value='unkill'; f.submit()} void(0);",
						confirm => '',
						off     =>
							$_REQUEST {fake} != -1
							,
					},

				]),

			}

		);
