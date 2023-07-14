CLASS zclhr_exprt_dyn_pay_serv DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    class-METHODS:
      "! Метод для примера. Метод обязательно (!) должен быть статичным, а иначе будет дамп
      "! @parameter is_rt | Входная структура, содержащая результат расчета
      "! @parameter it_wpbp | Основные выплаты
      "! @parameter et_rt | Выходная таблица с результатами
      example
        IMPORTING is_rt   TYPE pc207
                  it_wpbp TYPE hrpay99_wpbp
        EXPORTING et_rt   TYPE hrpay99_rt.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zclhr_exprt_dyn_pay_serv IMPLEMENTATION.
  METHOD example.
    " Метод для примера
  ENDMETHOD.

ENDCLASS.
