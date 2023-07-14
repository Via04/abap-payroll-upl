"! Интерфейс для хранения констант и методов начальной инициализации для программы экспорта ЗП
INTERFACE zif_exprt_payroll_constants
  PUBLIC .
  "! ТТ для записи мэппинга между историческим ВО и новым.<br/>
  "! <ol><li><em>LGART1</em> - код вида оплаты из системы выгрузки.</li>
  "!     <li><em>LGART2</em> - код вида оплаты из целевой системы.</li>
  "!     <li><em>LGTXT2</em> - текст вида оплаты.</li>
  "!     <li><em>BETPE</em> - Суммировать по полю? X/space</li>
  "!     <li><em>ANZHL</em> - Суммировать по полю? X/space</li>
  "!     <li><em>BETRG</em> - Суммировать по полю? X/space</li>
  "!     <li><em>CALL_METHOD</em> - метод для обработки вида оплаты</li></ol>
  TYPES ty_t_lgart_mapping TYPE STANDARD TABLE OF zhcm_exprt_lgmap WITH KEY primary_key COMPONENTS lgart1 lgart2.
  "! Тип данных для записи глубины выгрузки в строках
  TYPES ty_file_length(8) TYPE n.
  "! Тип данных для записи глубины анализа ТН
  TYPES ty_analysis_depth(2) TYPE n.
  "! ТТ RGDIR
  TYPES ty_t_rgdir TYPE STANDARD TABLE OF pc261 WITH DEFAULT KEY.
  TYPES:
    "! Структура для сохранения данных работы программы и дальнейшего сохранения в БД/вывода на экран.<br/>
    "! <ol><li><em>TBN</em> - ТН в исторической системе</li>
    "! <li><em>PERNR</em> - ТН в новой системе</li>
    "! <li><em>FPBEG</em> - начало FOR периода</li>
    "! <li><em>FPEND</em> - конец FOR периода</li>
    "! <li><em>CODE</em> - вид оплаты в исторической системе</li>
    "! <li><em>LGART</em> - вид оплаты в новой системе</li>
    "! <li><em>LGTEXT</em> - текст вида оплаты</li>
    "! <li><em>BETPE</em> - сумма ставок из RT по данному виду оплаты</li>
    "! <li><em>ANZHL</em> - количество из RT по данному виду оплаты</li>
    "! <li><em>BETRG</em> - сумма из RT по данному виду оплаты</li>
    "! <li><em>FIO</em> - Фамилия Имя Отчество</li></ol>
    BEGIN OF ty_out_data,
      tbn    TYPE persno,
      pernr  TYPE persno,
      fpbeg  TYPE fpbeg,
      fpend  TYPE fpend,
      code   TYPE lgart,
      lgart  TYPE lgart,
      lgtext TYPE lgtext,
      betpe  TYPE betpe,
      anzhl  TYPE pranz,
      betrg  TYPE maxbt,
      fio    TYPE emnam,
    END OF ty_out_data.
  TYPES ty_t_out_data_sorted TYPE SORTED TABLE OF ty_out_data WITH UNIQUE KEY primary_key COMPONENTS tbn pernr fpbeg fpend code lgart lgtext fio.
  TYPES ty_t_out_data TYPE STANDARD TABLE OF ty_out_data WITH DEFAULT KEY.
  TYPES ty_pernr_map TYPE zhr_migr_pernr.
  TYPES ty_t_pernr_map TYPE STANDARD TABLE OF zhr_migr_pernr WITH DEFAULT KEY.
  "! Количество месяцев для анализа и выгрузки. (глубина выгрузки)
  CONSTANTS ac_upl_depth TYPE ty_analysis_depth VALUE '24'.
  "! Группировка стран
  CONSTANTS ac_molga_33_rus TYPE molga VALUE '33'.
  "! Константа для поля система в файле, возможно потом изменится
  CONSTANTS ac_out_system_code(3) TYPE c VALUE 'TST'.
  CONSTANTS ac_dynamic_classname TYPE classname VALUE 'ZCLHR_EXPRT_DYN_PAY_SERV'.
  "! Метод вернет путь для загрузки файла по-умолчанию
  METHODS get_upl_def_path
    EXPORTING VALUE(ev_sep)  TYPE c
    RETURNING VALUE(rv_path) TYPE localfile
    RAISING   zcx_exprt_payroll_err.
  "! Метод вовращает таблицу мэппинга ВО
  METHODS get_lgart_mapping
    EXPORTING et_lga_map TYPE ty_t_lgart_mapping
    RAISING   zcx_exprt_payroll_err.

ENDINTERFACE.
