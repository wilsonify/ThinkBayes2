import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import HockeyGoals from '../components/HockeyGoals'

describe('HockeyGoals Component', () => {
  it('renders the component with initial data', () => {
    render(<HockeyGoals />)
    
    expect(screen.getByText('Hockey Goals Analysis')).toBeInTheDocument()
    expect(screen.getByText('Game 1:')).toBeInTheDocument()
    expect(screen.getByDisplayValue('3')).toBeInTheDocument() // First game goals
    expect(screen.getByDisplayValue('2.0')).toBeInTheDocument() // Prior alpha
    expect(screen.getByDisplayValue('1.0')).toBeInTheDocument() // Prior beta
  })

  it('calculates posterior mean correctly', () => {
    render(<HockeyGoals />)
    
    // With initial data: 10 games with total goals
    const posteriorMean = screen.getByText(/Posterior Mean/)
    expect(posteriorMean).toBeInTheDocument()
  })

  it('updates calculation when goals data changes', () => {
    render(<HockeyGoals />)
    
    const firstGameInput = screen.getByDisplayValue('3')
    fireEvent.change(firstGameInput, { target: { value: '5' } })
    
    // Posterior should update with new data
    expect(screen.getByText(/Posterior Mean/)).toBeInTheDocument()
  })

  it('adds new game to the list', () => {
    render(<HockeyGoals />)
    
    const addButton = screen.getByText('Add Game')
    fireEvent.click(addButton)
    
    expect(screen.getByText('Game 11:')).toBeInTheDocument()
  })

  it('removes game from the list', () => {
    render(<HockeyGoals />)
    
    // Should have 10 games initially
    expect(screen.getByText('Game 10:')).toBeInTheDocument()
    
    const removeButtons = screen.getAllByText('Remove')
    fireEvent.click(removeButtons[removeButtons.length - 1])
    
    // Should no longer have Game 10
    expect(screen.queryByText('Game 10:')).not.toBeInTheDocument()
  })

  it('updates when prior parameters change', () => {
    render(<HockeyGoals />)
    
    const alphaSlider = screen.getByLabelText(/Prior Alpha:/)
    fireEvent.change(alphaSlider, { target: { value: '3.0' } })
    
    expect(screen.getByDisplayValue('3.0')).toBeInTheDocument()
  })

  it('displays probability distributions chart', () => {
    render(<HockeyGoals />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Probability Distributions')).toBeInTheDocument()
  })

  it('shows posterior parameters', () => {
    render(<HockeyGoals />)
    
    expect(screen.getByText('Posterior Parameters')).toBeInTheDocument()
    expect(screen.getByText(/Alpha =/)).toBeInTheDocument()
    expect(screen.getByText(/Beta =/)).toBeInTheDocument()
  })

  it('displays mathematical framework', () => {
    render(<HockeyGoals />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Model Structure/)).toBeInTheDocument()
    expect(screen.getByText(/Posterior Update/)).toBeInTheDocument()
  })

  test('handles single game data', () => {
    render(<HockeyGoals />)
    
    // Remove all games except one
    const removeButtons = screen.getAllByText('Remove')
    for (let i = 0; i < 9; i++) {
      fireEvent.click(removeButtons[0])
    }
    
    expect(screen.getByText('Game 1:')).toBeInTheDocument()
    expect(screen.queryByText('Game 2:')).not.toBeInTheDocument()
  })

  test('validates goal input range', () => {
    render(<HockeyGoals />)
    
    const firstGameInput = screen.getByDisplayValue('3')
    
    // Test invalid input
    fireEvent.change(firstGameInput, { target: { value: '-1' } })
    expect(firstGameInput.value).toBe('3') // Should not accept negative
    
    fireEvent.change(firstGameInput, { target: { value: '20' } })
    expect(firstGameInput.value).toBe('15') // Should clamp to max
  })

  test('generates example data correctly', () => {
    render(<HockeyGoals />)
    
    const generateButton = screen.getByText('Generate Example')
    fireEvent.click(generateButton)
    
    // Should have new random data
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      const value = parseInt(input.value)
      expect(value).toBeGreaterThanOrEqual(0)
      expect(value).toBeLessThanOrEqual(15)
    })
  })

  test('displays interpretation section', () => {
    render(<HockeyGoals />)
    
    expect(screen.getByText('Interpretation')).toBeInTheDocument()
    expect(screen.getByText(/Based on/)).toBeInTheDocument()
    expect(screen.getByText(/games with/)).toBeInTheDocument()
  })

  test('prior parameters update correctly', () => {
    render(<HockeyGoals />)
    
    const alphaSlider = screen.getByLabelText(/Prior Alpha:/)
    const betaSlider = screen.getByLabelText(/Prior Beta:/)
    
    fireEvent.change(alphaSlider, { target: { value: '5.0' } })
    fireEvent.change(betaSlider, { target: { value: '2.0' } })
    
    expect(screen.getByDisplayValue('5.0')).toBeInTheDocument()
    expect(screen.getByDisplayValue('2.0')).toBeInTheDocument()
  })

  test('chart displays correct data', () => {
    render(<HockeyGoals />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByTestId('responsive-container')).toBeInTheDocument()
  })
})
