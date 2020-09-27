import React from 'react';

/**
 * jive.outcomes.summary.summaryContainer
 */
class SummaryContainer extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      subject: 'Summary Container!!'
    };
  }

  render() {
    return (
      <ul id="js-outcome-summary-container-{$objectType}-{$objectId}"
          className="js-outcome-summary-container j-outcome-summary-container js-ed-{$objectType}-{$objectId}"
          data-object-type="{$objectType}"
          data-object-id="{$objectId}"
          aria-label="{i18nText('outcomes.summaryContainer.ariaLabel')}"
          role="group">
      </ul>
    );
  }
}

export default SummaryContainer;
